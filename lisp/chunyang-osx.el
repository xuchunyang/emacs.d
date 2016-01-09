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

(defun omnifocus-new-entry (item)
  "Add ITEM to OmniFocus Inbox."
  (interactive "sItem: ")
  (do-applescript
   (format
    (concat
     "tell application \"OmniFocus\"\n"
     "    tell default document\n"
     "        parse tasks into it with transport text \"%s\"\n"
     "    end tell\n"
     "end tell\n")
    item)))

(provide 'chunyang-osx)
;;; chunyang-osx.el ends here
