;;; chunyang-mac.el --- macOS Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>

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

;; Some macOS supports

;;; Code:


;;; Working with Terminal.app

(defun chunyang-mac-cd-Terminal.app (dir)
  "Open Terminal.app and cd to DIR in it."
  (interactive "D")
  (do-applescript
   (format (concat
            "tell application \"Terminal\"\n"
            "activate\n"
            "do script \"cd %s\" in window 1\n"
            "end tell")
           dir)))

(provide 'chunyang-mac)
;;; chunyang-mac.el ends here
