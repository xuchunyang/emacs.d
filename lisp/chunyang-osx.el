;;; chunyang-osx.el --- OS X supports                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>

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

;; Some OS X supports

;; Don't use (even load this feature) if not on OS X.

;;; Code:

(defun system-move-file-to-trash (file) ; Trash support for OS X
  "Trash FILE using the Command-Line program \"trash\"."
  (call-process "trash" nil nil nil file))

(defun restart-emacs ()
  (interactive)
  ;; Make '~/.emacs.d/Restart-Emacs.scpt' as an OSX App firstly
  (shell-command "open -a Restart-Emacs"))

(defun omnifocus-new-entry-1 (action-title action-note)
  (do-applescript
   (format
    (concat
     "tell application \"OmniFocus\"\n"
     "    tell default document\n"
     "        make new inbox task with properties {name:\"%s\", note:\"%s\"}\n"
     "    end tell\n"
     "end tell\n")
    action-title action-note)))

(defun omnifocus-new-entry ()
  (interactive)
  (if (get-buffer "*OmniFocus*") (kill-buffer "*OmniFocus*"))
  (get-buffer-create "*OmniFocus*")
  (switch-to-buffer-other-window "*OmniFocus*")
  (setq-local header-line-format
              "Capture buffer.  Finish C-c C-c, abort C-c C-k")
  (local-set-key "\C-c\C-c"
                 (lambda () (interactive)
                   (let* ((lines (split-string (buffer-string) "\n"))
                          (action-title (or (car lines) ""))
                          (action-note (or (mapconcat
                                            #'identity
                                            (nthcdr 2 lines)
                                            "\n") "")))
                     (omnifocus-new-entry-1 action-title action-note)
                     (kill-this-buffer))))
  (local-set-key "\C-c\C-k" #'kill-this-buffer))


;;; Use Trackpad to switch to next/previous buffer
(use-package chunyang-osx-trackpad
  :disabled t                           ; Not work well
  :defer t
  :preface
  (require 'helm)
  (require 'dash)

  (defun chunyang-next-buffer-1 (&optional n)
    (let ((buffers
           (helm-skip-boring-buffers
            (mapcar #'buffer-name (buffer-list))
            nil)))
      (switch-to-buffer (car (-rotate (or n -1) buffers)) t)))

  (defun chunyang-previous-buffer-1 ()
    (chunyang-next-buffer-1 1))

  (defvar chunyang-next-buffer-timer nil)
  (defvar chunyang-previous-buffer-timer nil)

  (defun chunyang-next-buffer ()
    (interactive)
    (unless chunyang-next-buffer-timer
      (setq chunyang-next-buffer-timer
            (run-at-time 1 nil
                         (lambda ()
                           (cancel-timer chunyang-next-buffer-timer)
                           (setq chunyang-next-buffer-timer nil))))
      (message "=> NEXT buffer (%S)..."
               (key-description (this-command-keys)))
      (chunyang-next-buffer-1)))

  (defun chunyang-previous-buffer ()
    (interactive)
    (unless chunyang-previous-buffer-timer
      (setq chunyang-previous-buffer-timer
            (run-at-time 1 nil
                         (lambda ()
                           (cancel-timer chunyang-previous-buffer-timer)
                           (setq chunyang-previous-buffer-timer nil))))
      (message "=> PREV buffer (%S) ..."
               (key-description (this-command-keys)))
      (chunyang-previous-buffer-1))))

(provide 'chunyang-osx)
;;; chunyang-osx.el ends here
