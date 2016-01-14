;;; chunyang-buffers.el --- Utilities for buffers    -*- lexical-binding: t; -*-

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

;; Provide additional functions to manage buffers.

;; Base on code from https://gihub.com/lunaryorn/.emacs.d

;;; Code:

;; Don't kill the important buffers
(defconst lunaryorn-do-not-kill-buffer-names '("*scratch*" "*Messages*")
  "Names of buffers that should not be killed.")

(defun lunaryorn-do-not-kill-important-buffers ()
  "Inhibit killing of important buffers.

Add this to `kill-buffer-query-functions'."
  (if (not (member (buffer-name) lunaryorn-do-not-kill-buffer-names))
      t
    (message "Not allowed to kill %s, burying instead" (buffer-name))
    (bury-buffer)
    nil))


;;; Library

(defun buffer-lines (buffer-or-name &optional no-properties)
  "Return lines as a list in BUFFER-OR-NAME.

When the optional argument NO-PROPERTIES is non-nil,
strip text properties from the line."
  (split-string
   (with-current-buffer buffer-or-name
     (if no-properties
         (substring-no-properties (buffer-string))
       (buffer-string)))
   "\n"))

(defun buffer-longest-line (buffer-or-name &optional no-properties)
  "Return the (first) longest line in BUFFER-OR-NAME."
  (let (longest)
    (dolist (line (buffer-lines buffer-or-name no-properties) longest)
      (unless longest
        (setq longest line) )
      (when (< (length longest)
               (length line))
        (setq longest line)))))

(defun buffer-longest-linum (buffer-or-name)
  "Return the (first) longest line's line number in BUFFER-OR-NAME."
  (let (longest
        longest-linum
        (linum 0))
    (dolist (line (buffer-lines buffer-or-name) longest-linum)
      (setq linum (1+ linum))
      (unless longest
        (setq longest line
              longest-linum linum))
      (when (< (length longest)
               (length line))
        (setq longest line
              longest-linum linum)))))

(provide 'chunyang-buffers)
;;; chunyang-buffers.el ends here
