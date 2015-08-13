;;; chunyang-elisp.el --- Utilities for Emacs Lisp

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for Emacs Lisp

;;; Code:

(defvar chunyang-elisp-debug-buffer "*Debug ELisp Log*")

(defvar chunyang-elisp-debug nil
  "If non-nil, write log message into `chunyang-elisp-debug-buffer' buffer.")


;; Utility: logging
(defun chunyang-elisp-log (format-string &rest args)
  "Log message if `chunyang-elisp-debug-buffer' is non-nil.
Messages are written to  the `chunyang-elisp-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'.

Adapted from `helm-log'."
  (when chunyang-elisp-debug
    (with-current-buffer (get-buffer-create chunyang-elisp-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (setq-local inhibit-read-only t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format (concat (if (string-match "Start session" format-string)
                                    "* " "** ")
                                "%s.%06d (%s:%d)\n %s\n")
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (buffer-name (current-buffer)) (line-number-at-pos)
                        (apply #'format (cons format-string args))))))))

(defalias 'my-log #'chunyang-elisp-log)

(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
