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

(require 'subr-x)                       ; `string-empty-p'
(require 'cl-lib)                       ; `cl-case'

(defun chunyang-linux-gnome-terminal-cd (dir)
  "Invoke 'cd DIRECTORY' in a running GNOME Terminal, or open one if none."
  (interactive (list (expand-file-name
                      (if current-prefix-arg
                          (read-directory-name "cd to: ")
                        default-directory))))
  (let* ((out (shell-command-to-string
               "xdotool search --class 'Gnome-terminal' | tail -1"))
         (win (unless (string-empty-p out)
                ;; Remove trailing newline
                (substring out 0 -1))))
    (if win
        (let* ((keys (mapconcat
                      (lambda (c)
                        (cl-case c
                          ;; Note: use xev(1) to find key name
                          (?/ "slash")
                          (?. "period")
                          (?- "minus")
                          (t (char-to-string c))))
                      (string-to-list dir) " "))
               (cmd (format "xdotool windowactivate --sync %s key c d space %s Return" win keys)))
          (shell-command cmd))
      (let ((default-directory dir))
        (shell-command "gnome-terminal")))))

(provide 'chunyang-linux)
;;; chunyang-linux.el ends here
