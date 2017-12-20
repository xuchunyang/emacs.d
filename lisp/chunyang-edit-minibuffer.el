;;; chunyang-edit-minibuffer.el --- Edit Minibuffer in separate buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords:

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

;; Inspired by the string-edit package.

;;; Code:

(define-minor-mode chunyang-edit-minibuffer-mode
  "Minor mode for useful keybindings while editing minibuffer contents."
  nil
  " Edit Minibuffer"
  '(("\C-c\C-k" . chunyang-edit-minibuffer-abort)
    ("\C-c\C-c" . chunyang-edit-minibuffer-done)))

(defvar chunyang-edit-minibuffer-hook
  (list
   (lambda ()
     (pcase (minibuffer-prompt)
       ;; Used by `el-search-pattern' and `eval-expression'
       ((or "El-search pattern: " "Eval: ")
        (emacs-lisp-mode)))))
  "Hook to run just before enabling `chunyang-edit-minibuffer-mode'.
This hook provides an opportunity to enable a custom major mode
before the minor mode is enabled.

Since `chunyang-edit-minibuffer-mode-hook' doesn't work.")

;;;###autoload
(defun chunyang-edit-minibuffer ()
  "Edit the contents of minibuffer in separate buffer."
  (interactive)
  (if (minibufferp)
      (let ((text (minibuffer-contents))
            (buffer (get-buffer-create "*edit-minibuffer*")))
        (select-window
         (display-buffer
          buffer
          '(display-buffer-in-side-window . ((side . bottom)
                                             (window-height . 4)))))
        (erase-buffer)
        (insert text)
        (run-hooks 'chunyang-edit-minibuffer-hook)
        (chunyang-edit-minibuffer-mode))
    (user-error "The current buffer is not Minibuffer")))

(defun chunyang-edit-minibuffer-abort ()
  (interactive)
  (kill-buffer)
  (select-window (active-minibuffer-window)))

(defun chunyang-edit-minibuffer-done ()
  (interactive)
  (let ((text (buffer-string)))
    (kill-buffer)
    (select-window (active-minibuffer-window))
    (delete-minibuffer-contents)
    (insert text)))

(provide 'chunyang-edit-minibuffer)
;;; chunyang-edit-minibuffer.el ends here
