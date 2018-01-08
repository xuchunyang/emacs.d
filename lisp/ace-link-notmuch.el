;;; ace-link-notmuch.el --- ace-link support for notmuch  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Package-Requires: ((avy "0.2.0"))
;; Keywords: notmuch, link

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides ace-link support for Notmuch's `notmuch-hello-mode'
;; and `notmuch-show-mode'.
;;
;; Use `ace-link-notmuch-setup' to set up the default bindings.

;;; Code:

(require 'avy)
(require 'wid-edit)

(defun ace-link--notmuch-hello-collect ()
  "Collect the positions of visible links in *notmuch-hello*."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (- (window-end) 57))
        (goto-char (point-min))
        (forward-line 3)
        (setq pt (point))
        (while (progn (widget-forward 1)
                      (> (point) pt))
          (setq pt (point))
          (when (get-char-property (point) 'button)
            (push (point) candidates)))))
    (nreverse candidates)))

(defun ace-link--notmuch-hello-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (widget-button-press (point))))

(defun ace-link-notmuch-hello ()
  "Open a visible link in *notmuch-hello*."
  (interactive)
  (let ((pt (avy-with ace-link-notmuch-hello
              (avy--process
               (ace-link--notmuch-hello-collect)
               #'avy--overlay-pre))))
    (ace-link--notmuch-hello-action pt)))

(defun ace-link--notmuch-show-collect ()
  "Collect the positions of visible links in `notmuch-show' buffer."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (setq pt (- (point) (length (match-string 0))))
          (push pt candidates))))
    (nreverse candidates)))

(defun ace-link--notmuch-show-action  (pt)
  (goto-char pt)
  (browse-url-at-point))

(defun ace-link-notmuch-show ()
  "Open a visible link in `notmuch-show' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-notmuch-show
              (avy--process
               (ace-link--notmuch-show-collect)
               #'avy--overlay-pre))))
    (ace-link--notmuch-show-action pt)))

;;;###autoload
(defun ace-link-notmuch-setup (&optional key)
  "Bind KEY to appropriate functions in appropriate keymaps."
  (setq key (or key "o"))
  (eval-after-load "notmuch-hello"
    `(define-key notmuch-hello-mode-map ,key 'ace-link-notmuch-hello))
  (eval-after-load "notmuch-show"
    `(define-key notmuch-show-mode-map ,key 'ace-link-notmuch-show)))

(provide 'ace-link-notmuch)
;;; ace-link-notmuch.el ends here
