;;; chunyang-linux.el --- GNU/Linux Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: linux

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

(defun chunyang-open-gnome-terminal ()
  (interactive)
  (let ((dir default-directory)
        (cmd
         "xdotool windowactivate --sync $(xdotool search --class 'Gnome-terminal' | tail -1) key c d space %s Return"))
    (shell-command (format cmd (mapconcat
                                (lambda (c)
                                  (cl-case c
                                    ;; Note: use xev(1) to find key name
                                    (?/ "slash")
                                    (?. "period")
                                    (t (char-to-string c))))
                                (string-to-list dir) " ")))))

(provide 'chunyang-linux)
;;; chunyang-linux.el ends here
