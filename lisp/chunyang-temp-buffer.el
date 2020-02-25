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
(defun chunyang-temp-buffer (mode)
  (interactive
   (list
    (intern-soft         ; or `intern'? I don't really understand the difference
     (completing-read
      "Major Mode: "
      (delete-dups
       (mapcar #'cdr auto-mode-alist))))))
  (let* ((regexp (car (rassq mode auto-mode-alist)))
         (guess-extension (cadr (xr regexp)))
         (filename (format "%s%s"
                           (format-time-string "%Y-%m-%dT%H%M")
                           guess-extension)))
    (unless (string-match-p regexp filename)
      (error "Filename %S doesn't pattern %S" filename regexp))
    (find-file (expand-file-name filename chunyang-temp-buffer-dir))))


(provide 'chunyang-temp-buffer)
;;; chunyang-temp-buffer.el ends here
