;;; canonicalize.el --- Change github into GitHub and so on  -*- lexical-binding: t; -*-

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

(defconst canonicalize-file (expand-file-name
                             "canonicalize.txt"
                             (file-name-directory
                              (or load-file-name buffer-file-name)))
  "The full path to the directory file.")

(defvar canonicalize-alist nil
  "A list of (\"github\" . \"GitHub\").")

(defvar canonicalize-alist-cache-time nil)

(defun canonicalize-alist ()
  (let ((mt (file-attribute-modification-time
             (file-attributes canonicalize-file))))
    (when (or (not canonicalize-alist-cache-time)
              (time-less-p canonicalize-alist-cache-time mt))
      (setq canonicalize-alist-cache-time mt)
      (setq canonicalize-alist
            (mapcar (lambda (w) (cons (downcase w) w)) (canonicalize-file-read)))))
  canonicalize-alist)

(defun canonicalize-file-read ()
  (with-temp-buffer
    (insert-file-contents canonicalize-file)
    (and (looking-at-p ";;;") (forward-line))
    (let (words)
      (while (not (eobp))
        (push (buffer-substring (line-beginning-position) (line-end-position))
              words)
        (forward-line))
      (nreverse words))))

;;;###autoload
(defun canonicalize-dwim (beg end)
  (declare (interactive-only t))
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (let ((bounds (bounds-of-thing-at-point 'word)))
                   (if bounds
                       (list (car bounds) (cdr bounds))
                     (user-error "No region or word found")))))
  (let* ((word (buffer-substring beg end))
         (alist (canonicalize-alist))
         (repl (cdr (assoc (downcase word) alist #'string=))))
    (cond ((equal word repl))
          (repl (delete-region beg end)
                (insert repl))
          (t (capitalize-region beg end)))))

(provide 'canonicalize)
;;; canonicalize.el ends here
