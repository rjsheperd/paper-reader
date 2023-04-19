;;; paper-reader.el --- Emacs Client for Instapaper -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Richard J. Sheperd <rjsheperd@gmail.com>
;; Created: 2023-04-19
;; Version: 0.1-pre
;; Keywords: instapaper
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/alphapapa/pocket-reader.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; paper-reader.el is a set of functions to add urls to Instapaper.
;; Instapaper is at https://www.instapaper.com/. This is not an official
;; instapaper client.

;; Note that passwords are not required on instapaper. You must have
;; an instapaper account to use this package; it will not create one
;; for you.

;;; Setup:
;; - Add your credentials to ~/.authinfo.gpg:
;;   ~machine instapaper.com login <EMAIL> password <PASSWORD>~
;; - Add the recommended keybindings:
;;   (global-set-key (kbd "C-c i a") 'paper-reader-add)
;;   (global-set-key (kbd "C-c i i") 'paper-reader-add-at-point))

;;; Usage:
;; - ~M-x paper-reader-add / C-c i a~ to add a URL to Instapaper
;; - ~M-x paper-reader-add-at-point / C-c i i~ to add the URL your cursor is on to Instapaper (works in Org, Elfeed, w3m and eww)

;; Changelog
;; 0.1 - Add custom point saving for org/eww/w3m/shr/elfeed, leverage auth-source package

;;; Code:

;;;; Requirements

(require 'url)
(require 'browse-url)
(require 'auth-source)

;;;; Variables

(defvar paper-reader-api-base "https://www.instapaper.com/api/"
  "Base URL for all Instapaper API functions")
(defvar paper-reader-auth-url (concat paper-api-base "authenticate")
  "URL for method for validating an Instapaper username and password")
(defvar paper-reader-add-url (concat paper-api-base "add")
  "URL for method for adding a URL to Instapaper")

(defconst paper-reader-version "0.1"
  "Version of paper-reader.el")

;;;;; Customization

(defcustom paper-reader-username ""
  "Username or email address to use for authentication"
  :type 'string
  :group 'instapaper)

(defcustom paper-reader-password ""
  "Password (if any) to use for authentication"
  :type 'string
  :group 'instapaper)

;;;; Functions

;; Define the callback function for `url-retrieve`
(defun paper-reader-add-callback (status &optional url)
  "Callback function for `url-retrieve`. STATUS is the HTTP response."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9].[0-9] \\([0-9]+\\)" nil t)
      (let ((status-code   (string-to-number (match-string 1)))
	    (response-body (buffer-substring-no-properties (point-min) (point-max))))
	(if (< status-code 300)
	    (message "Saved to Instapaper! (%s)" url)
	  (message "[ERROR] STATUS: '%s' \n\n RESPONSE: '%s'" status-code response-code))))))

(defun paper-reader-lib-add (url &optional title selection)
  "Add url to instapaper"
  (let* ((api-key-pair (car (auth-source-search :max 1 :host "instapaper.com" :require '(:user :secret))))
         (paper-reader-username (plist-get api-key-pair :user))
         (paper-reader-password (funcall (plist-get api-key-pair :secret)))
         (url-request (concat paper-reader-add-url
                              "?username=" (url-hexify-string paper-reader-username) "&"
                              "password="  (url-hexify-string paper-reader-password) "&"
                              "url="       (url-hexify-string url) "&")))
    (url-retrieve url-request #'paper-reader-add-callback (list url))))

;;;;; Commands

;;;###autoload
(defun paper-reader-add (url)
  "Add url to instapaper"
  (interactive "sURL: ")
  (paper-reader-lib-add url))

;;;###autoload
(defun paper-reader-add-at-point ()
  "Add link at point to Instapaper. This function tries to work in multiple major modes, such as w3m, eww, elfeed, and Org."
  (interactive)
  (cl-case major-mode
    ('eww-mode (paper-reader-eww-add-link))
    ('org-mode (paper-reader-org-add-link))
    ('w3m-mode (paper-reader-w3m-add-link))
    ('shr-mode (paper-reader-shr-add-link))
    ('elfeed-search-mode (paper-reader-elfeed-search-add-link))
    ('elfeed-show-mode (paper-reader-elfeed-entry-add-link))
    (t (paper-reader-generic-add-link))))

;;;###autoload
(defun paper-reader-eww-add-link ()
  "Add link at point to Instapaper in eww buffers."
  (interactive)
  ;; `eww-links-at-point' returns a list of links, but we only use the
  ;; first one.  I think this is the right thing to do in most, if not
  ;; all, cases.
  (when-let ((url (car (eww-links-at-point))))
    (paper-reader-lib-add url)))

;;;###autoload
(defun paper-reader-org-add-link ()
  "Add link at point to Instapaper in Org buffers."
  (interactive)
  (if-let ((url (when (org-in-regexp org-bracket-link-regexp 1)
                    (org-link-unescape (match-string-no-properties 1)))))
      (paper-reader-lib-add url)
    (paper-reader-generic-add-link)))

(declare-function 'w3m-with-lnum 'w3m-lnum)
(declare-function 'w3m-lnum-read-interactive 'w3m-lnum)
(declare-function 'w3m-lnum-get-anchor-info 'w3m-lnum)
(defvar last-index)
(defvar w3m-current-url)
;;;###autoload
(with-eval-after-load 'w3m-lnum
  (cl-defun paper-reader-w3m-lnum-add-link (&key (type 1))
    "Add link to Instapaper with lnum in w3m buffers."
    (interactive)
    (w3m-with-lnum
     type ""
     (when-let ((num (car (w3m-lnum-read-interactive
                           "Anchor number: "
                           'w3m-lnum-highlight-anchor
                           type last-index w3m-current-url)))
                (info (w3m-lnum-get-anchor-info num))
                (url (car info)))
       (paper-reader-lib-add url)))))

;;;###autoload
(with-eval-after-load 'w3m
  (defun paper-reader-w3m-add-link ()
    "Add link at point to Instapaper in w3m buffers."
    (interactive)
    (if-let ((url (or (get-text-property (point) 'w3m-href-anchor)
                      (unless (bolp)
                        (save-excursion
                          (get-text-property (1- (point)) 'w3m-href-anchor)))
                      (unless (eolp)
                        (save-excursion
                          (get-text-property (1+ (point)) 'w3m-href-anchor)))
                      (thing-at-point-url-at-point))))
        (paper-reader-lib-add url)
      (if (member 'w3m-lnum-mode minor-mode-list)
          ;; No URL found around point: use lnum if loaded
          (paper-reader-w3m-lnum-add-link)
        ;; We tried.
        (message "No URL found around point.")))))

;;;###autoload
(defun paper-reader-shr-add-link ()
  "Add link at point in `shr-mode' buffer to Instapaper."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (paper-reader-lib-add url)))

(defvar elfeed-show-entry)
;;;###autoload
(with-eval-after-load 'elfeed
  (defun paper-reader-elfeed-search-add-link ()
    "Add links for selected entries in Elfeed search-mode buffer to Instapaper.
This is only for the elfeed-search buffer, not for entry buffers."
    (interactive)
    (when-let ((entries (elfeed-search-selected))
               (links (mapcar #'elfeed-entry-link entries)))
      (mapcar #'paper-reader-lib-add-url links)))

  (defun paper-reader-elfeed-entry-add-link ()
    "Add links for selected entries in elfeed-show-mode buffer to Instapaper.
This is only for the elfeed-entry buffer, not for search buffers."
    (interactive)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (paper-reader-lib-add link))))

;;;###autoload
(defun paper-reader-generic-add-link ()
  "Try to add URL at point to Instapaper using `thing-at-pt'."
  (interactive)
  (if-let ((url (or (thing-at-point-url-at-point)
                    (with-temp-buffer
                      (insert (gui-get-selection))
                      (thing-at-point-url-at-point)))))
      (paper-reader-lib-add url)
    (user-error "No URL found at point or in clipboard")))

;;;; Footer

(provide 'paper-reader)

;;; paper-reader.el ends here
