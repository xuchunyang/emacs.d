;;; longman-3000.el --- Longman 3000                      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: english

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

(require 'subr-x)

;; https://github.com/sapbmw/Longman-Communication-3000
(defconst longman-3000-txt
  "~/src/Longman-Communication-3000/Longman_Communication_3000.txt")

(defvar longman-3000 nil)

(defun longman-3000 ()
  (unless longman-3000
    (with-temp-buffer
      (insert-file-contents longman-3000-txt)
      (while (not (eobp))
        (push (string-trim (buffer-substring (line-beginning-position) (line-end-position)))
              longman-3000)
        (forward-line))
      ;; remove "=== end ==="
      (pop longman-3000)
      (setq longman-3000 (nreverse longman-3000))))
  longman-3000)

;; IDEA: 要是能加上简单释义就好了，可参照 lazycat 的 company english

;;;###autoload
(defun longman-3000-insert ()
  (interactive)
  (insert (completing-read "Longman 3000: " (longman-3000))))

;;;###autoload
(defun longman-3000-helm ()
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "Longman 3000"
     :candidates (longman-3000)
     :action (helm-make-actions "Insert" #'insert))))

;;;###autoload
(defun longman-3000-company (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    ('interactive (company-begin-backend 'longman-3000-company))
    ('prefix (company-grab-word))
    ('candidates
     (let ((case-fold-search t))
       (cl-loop for w in (longman-3000)
                if (string-match (regexp-quote arg) w)
                collect w)))
    ('sorted t)
    ('ignore-case 'keep-prefix)))

(provide 'longman)
;;; longman-3000.el ends here
