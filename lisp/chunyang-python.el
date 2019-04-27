;;; chunyang-python.el --- My Python helpers         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;;;###autoload
(defun chunyang-jedi ()
  (interactive)
  (let ((line (line-number-at-pos))
        (column (current-column))
        (file buffer-file-name)
        (tmpfile (make-temp-file "chunyang-jedi-"))
        (prompt (buffer-substring (line-beginning-position) (point)))
        (orig-buffer (current-buffer)))
    (write-region nil nil tmpfile)
    (with-temp-buffer
      (if (zerop (call-process "jedi-completions.py" tmpfile t nil
                               file
                               (number-to-string line)
                               (number-to-string column)))
          (let ((completions (split-string (buffer-string) "\n" t)))
            ;; TODO try in-buffer completions instead.
            ;; See (info "(elisp) Completion in Buffers")
            (with-current-buffer orig-buffer
              (insert (completing-read prompt completions nil t))))
        (message "Error:\n%s" (buffer-string))))
    (delete-file tmpfile)))

;;;###autoload
(defun chunyang-python-comment-box (b e)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-end-position))))
  (let ((s (string-trim (buffer-substring b e))))
    (delete-region b e)
    ;; NOTE PEP8 要求 # 后得有一个空格，但我不想要一个坏的盒子
    (insert
     "#*------------------------------------------------------------------#\n"
     "#*    " s (make-string (- 69 6 (string-width s) 1) ?\s)            "#\n"           
     "#*------------------------------------------------------------------#\n")))

(provide 'chunyang-python)
;;; chunyang-python.el ends here

