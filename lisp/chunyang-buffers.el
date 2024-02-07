;;; chunyang-buffers.el --- Utilities for buffers    -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;; Provide additional functions to manage buffers.

;;; Code:

;; Add this to `kill-buffer-query-functions'
(defun chunyang-kill-buffer-query-function ()
  ;; t is kill, nil is not kill
  (not (member (buffer-name) '("*scratch*" "*Messages*"))))


;;; Commands

;; My answer to
;; http://emacs.stackexchange.com/questions/21149/let-switch-to-buffer-to-already-open-buffer-switch-to-that-window-rather-than-o
;; I keep it here since it looks like a useful alternative to
;; `switch-to-buffer', though now I only use `helm-mini'.
(defun chunyang-switch-to-buffer (buffer)
  "Display BUFFER in the selected window.
If BUFFER is displayed in some window, select that window instead."
  (interactive
   (list (get-buffer (read-buffer
                      "Switch to buffer: "
                      (other-buffer (current-buffer))))))
  (cond
   ((eq buffer (window-buffer)))
   (t (let ((win (get-buffer-window buffer)))
        (if win
            (select-window win)
          (switch-to-buffer buffer))))))

;; Another solution (which is more general)
;;
;; (define-advice switch-to-buffer (:around (orig-fun &rest args) hack)
;;   (if-let ((win (get-buffer-window (car args))))
;;       (select-window win)
;;     (apply orig-fun args)))


;;; Copy `buffer-file-name'

(defun chunyang-copy-buffer-file-name (&optional buffer)
  "Save filename of BUFFER is visiting to kill-ring."
  (interactive)
  (let ((file (buffer-file-name buffer)))
    (if file
        (progn (kill-new file)
               (message "Copied: %s" file))
      (user-error "Not visiting a file"))))


;;; Reopen last closed file

;; Adapted from https://emacs-china.org/t/topic/3318

(defvar chunyang-last-closed-file-list nil)

(defun chunyang-last-closed-file-track ()
  (when buffer-file-name
    (push buffer-file-name chunyang-last-closed-file-list)))

(defun chunyang-last-closed-file-reopen ()
  (interactive)
  (if chunyang-last-closed-file-list
      (find-file (pop chunyang-last-closed-file-list))
    (user-error "No last closed file to reopen")))

(define-minor-mode chunyang-last-closed-file-mode
  "Reopen last closed file."
  :global t
  (if chunyang-last-closed-file-mode
      (add-hook 'kill-buffer-hook #'chunyang-last-closed-file-track)
    (remove-hook 'kill-buffer-hook #'chunyang-last-closed-file-track)))

(provide 'chunyang-buffers)
;;; chunyang-buffers.el ends here
