;;; macports.el --- MacPorts Interface               -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: MacPorts

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

;;

;;; Code:

(defun macports-list-port ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*MacPorts*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (shell-command "port list" (current-buffer))))

(provide 'macports)
;;; macports.el ends here
