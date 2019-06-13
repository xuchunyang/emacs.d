;;; tldr2.el --- Yet Another tldr-packages client    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/emacs.d/lisp/tldr2.el
;; Created: 2019年 6月12日 星期三 19时36分44秒 CST
;; Package-Requires: ((emacs "26.1"))
;; Version: 0

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

;; Yet another Emacs client for [tldr](https://github.com/tldr-pages/tldr).

;;; Code:

(defvar tldr2-dir "~/src/tldr"
  "The directory contains pages/.")

(defun tldr2-commands ()
  "/Users/xcy/src/tldr/pages/common/7z.md"
  (let (commands)
    (dolist (path (mapcan
                   (lambda (d)
                     (directory-files-recursively d (rx ".md" eos)))
                   (directory-files tldr2-dir 'full (rx bos "pages"))))
      ;; /Users/xcy/src/tldr/pages/common/7z.md
      (pcase-exhaustive path
        ((rx "pages"
             (opt "." (let lang (1+ anything))) "/"
             (let platform (1+ anything)) "/"
             (let command (1+ anything)) ".md"
             eos)
         (setq lang (or lang "en"))
         (if-let ((x (cdr (assoc command commands))))
             (progn
               (cl-pushnew platform (alist-get 'platform x) :test #'string=)
               (push lang (alist-get 'lang x)))
           (push `(,command . ((lang . (,lang))
                               (platform . (,platform))))
                 commands)))))
    (nreverse commands)))

(defun tldr2-file (command platform lang)
  (expand-file-name
   (format "pages%s/%s/%s.md"
           (pcase lang
             ("en" "")
             (_ (concat "." lang)))
           platform
           command)
   tldr2-dir))

;; (find-file (tldr2-file "ls" "common" "en"))

;; Maybe it is easier to M-x find-file directly?
(defun tldr2-find-file (command platform lang)
  (interactive
   (let* ((commands (tldr2-commands))
          (command (completing-read "Command: " commands nil t))
          (alist (cdr (assoc command commands)))
          (platform (completing-read "Platform: " (alist-get 'platform alist)))
          (lang (completing-read "Lang: " (alist-get 'lang alist))))
     (list command platform lang)))
  (find-file (tldr2-file command platform lang)))

;; IDEA helm and eshell support

(provide 'tldr2)
;;; tldr2.el ends here
