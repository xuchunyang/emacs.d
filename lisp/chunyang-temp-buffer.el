;;; chunyang-temp-buffer.el --- 临时 Buffer，持久 File  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Package-Requires: ((xr "1.15"))
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

(require 'xr)

(defvar chunyang-temp-buffer-dir "~/Temp")

;;;###autoload
(defun chunyang-temp-buffer (filename)
  (interactive
   (list
    (read-string
     "Filename: "
     (expand-file-name (format-time-string "%Y-%m-%dT%H%M.")
                       chunyang-temp-buffer-dir))))
  (find-file filename))

(provide 'chunyang-temp-buffer)
;;; chunyang-temp-buffer.el ends here
