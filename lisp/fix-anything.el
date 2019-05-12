;;; fix-anything.el --- Fix anything                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
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

;;

;;; Code:

(defun fix-anything ()
  (interactive)
  ;; Source file `/Users/xcy/.emacs.d/lisp/ace-link-notmuch.el' newer than byte-compiled file
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (when (string-match "^Source file `\\(.*\\)' newer than byte-compiled file$" line)
      (let ((filename (match-string 1 line)))
        (message "Byte-compiling and load %s" filename)
        (byte-compile-file filename 'load)))))

(provide 'fix-anything)
;;; fix-anything.el ends here
