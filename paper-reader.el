;; paper-reader.el --- Instapaper client from emacs
;; Copyright (C) 2022 Richard J. Sheperd

;; Author: Richard J. Sheperd <rjsheperd@gmail.com>
;; Last update: 2022-03-22
;; Version: 0.1
;; URL: htts://github.com/rjsheperd/paper-reader
;; Contributors:

;; Inspiration:
;;  - Jason F. McBrayer <jmcbray@carcosa.net> with instapaper.el (https://github.com/emacsmirror/instapaper)
;;  - Adam Porter <adam@alphapapa.net> with pocket-reader.el (https://github.com/alphapapa/pocket-reader.el)

;; paper-reader.el is a set of functions to add urls to instapaper, a
;; simple tool to save web pages for reading later. Instapaper is at
;; https://www.instapaper.com/. This is not an official instapaper
;; client.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Requirements:
;; - auth-source
;; - browse-url

;; Installation:
;; - From MELPA: ~M-x package-install paper-reader~
;; - Ensure use-package is installed
;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))

;; - Add this to your init.el
;;   (use-package paper-reader
;;     :ensure t
;;     :config
;;     ;; Recommended keybindings
;;     (global-set-key (kbd "C-c i a") 'paper-add)
;;     (global-set-key (kbd "C-c i i") 'paper-add-at-point))
;;
;; - Add your credentials to ~/.authinfo.gpg
;; machine instapaper.com login <EMAIL> password <PASSWORD>

;; Usage
;; - ~M-x paper-add / C-c i a~ to add a URL to Instapaper
;; - ~M-x paper-add-at-point / C-c i i~ to add the URL your cursor is on to Instapaper (works in Org, Elfeed, w3m and eww)

;; Note that passwords are not required on instapaper. You must have
;; an instapaper account to use this package; it will not create one
;; for you.

;; Changelog
;; 0.1 - Add custom point saving for org/eww/w3m/shr/elfeed, leverage auth-source package

(require 'url)
(require 'browse-url)
(require 'auth-source)

(defvar paper-api-base "https://www.instapaper.com/api/"
  "Base URL for all Instapaper API functions")
(defvar paper-auth-url (concat paper-api-base "authenticate")
  "URL for method for validating an Instapaper username and password")
(defvar paper-add-url (concat paper-api-base "add")
  "URL for method for adding a URL to Instapaper")

(defconst paper-reader-version "0.1"
  "Version of paper-reader.el")

(defcustom paper-username ""
  "Username or email address to use for authentication"
  :type 'string
  :group 'instapaper)

(defcustom paper-password ""
  "Password (if any) to use for authentication"
  :type 'string
  :group 'instapaper)

;; Define the callback function for `url-retrieve`
(defun paper-add-callback (status &optional url)
  "Callback function for `url-retrieve`. STATUS is the HTTP response."
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9].[0-9] \\([0-9]+\\)" nil t)
      (let ((status-code   (string-to-number (match-string 1)))
	    (response-body (buffer-substring-no-properties (point-min) (point-max))))
	(if (< status-code 300)
	    (message "Saved to Instapaper! (%s)" url)
	  (message "[ERROR] STATUS: '%s' \n\n RESPONSE: '%s'" status-code response-code))))))

(defun paper-lib-add (url &optional title selection)
  "Add url to instapaper"
  (let* ((api-key-pair (car (auth-source-search :max 1 :host "instapaper.com" :require '(:user :secret))))
         (paper-username (plist-get api-key-pair :user))
         (paper-password (funcall (plist-get api-key-pair :secret)))
         (url-request (concat paper-add-url
                              "?username=" (url-hexify-string paper-username) "&"
                              "password="  (url-hexify-string paper-password) "&"
                              "url="       (url-hexify-string url) "&")))
    (url-retrieve url-request #'paper-add-callback (list url))))

;;;###autoload
(defun paper-add (url)
  "Add url to instapaper"
  (interactive "sURL: ")
  (paper-lib-add url))

;;;;; URL-adding helpers

;;;###autoload
(defun paper-add-at-point ()
  "Add link at point to Instapaper. This function tries to work in multiple major modes, such as w3m, eww, elfeed, and Org."
  (interactive)
  (cl-case major-mode
    ('eww-mode (paper-eww-add-link))
    ('org-mode (paper-org-add-link))
    ('w3m-mode (paper-w3m-add-link))
    ('shr-mode (paper-shr-add-link))
    ('elfeed-search-mode (paper-elfeed-search-add-link))
    ('elfeed-show-mode (paper-elfeed-entry-add-link))
    (t (paper-generic-add-link))))

;;;###autoload
(defun paper-eww-add-link ()
  "Add link at point to Instapaper in eww buffers."
  (interactive)
  ;; `eww-links-at-point' returns a list of links, but we only use the
  ;; first one.  I think this is the right thing to do in most, if not
  ;; all, cases.
  (when-let ((url (car (eww-links-at-point))))
    (paper-lib-add url)))

;;;###autoload
(defun paper-org-add-link ()
  "Add link at point to Instapaper in Org buffers."
  (interactive)
  (if-let ((url (when (org-in-regexp org-bracket-link-regexp 1)
                    (org-link-unescape (match-string-no-properties 1)))))
      (paper-lib-add url)
    (paper-generic-add-link)))

(declare-function 'w3m-with-lnum 'w3m-lnum)
(declare-function 'w3m-lnum-read-interactive 'w3m-lnum)
(declare-function 'w3m-lnum-get-anchor-info 'w3m-lnum)
(defvar last-index)
(defvar w3m-current-url)
;;;###autoload
(with-eval-after-load 'w3m-lnum
  (cl-defun paper-w3m-lnum-add-link (&key (type 1))
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
       (paper-lib-add url)))))

;;;###autoload
(with-eval-after-load 'w3m
  (defun paper-w3m-add-link ()
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
        (paper-lib-add url)
      (if (member 'w3m-lnum-mode minor-mode-list)
          ;; No URL found around point: use lnum if loaded
          (paper-w3m-lnum-add-link)
        ;; We tried.
        (message "No URL found around point.")))))

;;;###autoload
(defun paper-shr-add-link ()
  "Add link at point in `shr-mode' buffer to Instapaper."
  (interactive)
  (if-let ((url (get-text-property (point) 'shr-url)))
      (paper-lib-add url)))

(defvar elfeed-show-entry)
;;;###autoload
(with-eval-after-load 'elfeed
  (defun paper-elfeed-search-add-link ()
    "Add links for selected entries in Elfeed search-mode buffer to Instapaper.
This is only for the elfeed-search buffer, not for entry buffers."
    (interactive)
    (when-let ((entries (elfeed-search-selected))
               (links (mapcar #'elfeed-entry-link entries)))
      (mapcar #'paper-lib-add-url links)))

  (defun paper-elfeed-entry-add-link ()
    "Add links for selected entries in elfeed-show-mode buffer to Instapaper.
This is only for the elfeed-entry buffer, not for search buffers."
    (interactive)
    (when-let ((link (elfeed-entry-link elfeed-show-entry)))
      (paper-lib-add link))))

;;;###autoload
(defun paper-generic-add-link ()
  "Try to add URL at point to Instapaper using `thing-at-pt'."
  (interactive)
  (if-let ((url (or (thing-at-point-url-at-point)
                    (with-temp-buffer
                      (insert (gui-get-selection))
                      (thing-at-point-url-at-point)))))
      (paper-lib-add url)
    (user-error "No URL found at point or in clipboard")))

;;; Footer

(provide 'paper-reader)
