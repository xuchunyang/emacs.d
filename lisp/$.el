;;; $.el --- $                                       -*- lexical-binding: t; -*-

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

(require 'rx)

(defun $ (pattern)
  "Run shell command in the current line and insert the output."
  (interactive
   (list (buffer-substring-no-properties
          (line-beginning-position)
          (line-end-position))))
  (let (directory command)
    (pcase pattern
      ((rx bos "$ " (let cmd (1+ anything)) eos)
       (setq directory default-directory
             command cmd))
      ((rx bos (let dir (1+ (not (in " ")))) " $ " (let cmd (1+ anything)) eos)
       (setq directory dir
             command cmd))
      (_ (user-error "Unrecognized format, support '$ date' and '~/.emacs.d $ pwd'")))
    (save-excursion
      ;; Delete old output
      (delete-region (progn (forward-line)
                            (point))
                     (progn (while (get-text-property (point) '$)
                              (forward-line))
                            (point)))
      (unless (bolp) (insert "\n"))
      (insert
       (with-temp-buffer
         (let ((default-directory directory))
           (shell-command command t)

           (propertize (buffer-string) '$ t 'rear-nonsticky t)))))))

(provide '$)
;;; $.el ends here
