;;; eshell-z.el --- jump around

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Package-Requires: ((emacs "24.4") (seq "1.0"))
;; Keywords: convenience
;; Version: 0.1
;; Homepage: https://github.com/xuchunyang/eshell-z

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

(require 'eshell)
(require 'em-dirs)
(require 'subr-x)

(defcustom eshell-z-freq-dir-hash-table-file-name
  (expand-file-name "freqdir" eshell-directory-name)
  "If non-nil, name of the file to read/write the freq-dir-hash-table.
If it is nil, the freq-dir-hash-table will not be written to disk."
  :type 'file
  :group 'eshell-dirs)

(defvar eshell-z-freq-dir-hash-table nil
  "The frequent directory that Eshell was in.")

(defun eshell-z--read-freq-dir-hash-table ()
  "Set `eshell-z-freq-dir-hash-table' from a history file."
  (let ((file eshell-z-freq-dir-hash-table-file-name))
    (cond
     ((or (null file)
          (equal file "")
          (file-directory-p file)
          (not (file-readable-p file)))
      nil)
     (t
      (setq eshell-z-freq-dir-hash-table (with-temp-buffer
                                           (insert-file-contents file)
                                           (read (current-buffer))))))))

(defun eshell-z--write-freq-dir-hash-table ()
  "Write `eshell-z-freq-dir-hash-table' to a history file."
  (let ((file eshell-z-freq-dir-hash-table-file-name))
    (cond
     ((or (null file)
          (equal file "")
          (null eshell-z-freq-dir-hash-table)
          (hash-table-empty-p eshell-z-freq-dir-hash-table))
      nil)
     ((and (file-exists-p file)
           (not (file-directory-p file))
           (not (file-writable-p file)))
      (message "Cannot write freq-dir-hash-table file %s" file))
     (t
      (with-temp-buffer
        (print eshell-z-freq-dir-hash-table (current-buffer))
        (write-region (point-min) (point-max) file))))))

(add-hook 'eshell-exit-hook 'eshell-z--write-freq-dir-hash-table)

(defun eshell-z--add ()
  "Add entry."
  (if eshell-z-freq-dir-hash-table-file-name
      (eshell-z--read-freq-dir-hash-table))
  (unless eshell-z-freq-dir-hash-table
    (setq eshell-z-freq-dir-hash-table (make-hash-table :test 'equal)))
  ;; $HOME isn't worth matching
  (unless (string= (expand-file-name default-directory)
                   (expand-file-name "~/"))
    (if-let ((key default-directory)
             (val (gethash key eshell-z-freq-dir-hash-table)))
        (puthash key (cons key
                           (list :freq (1+ (plist-get (cdr val) :freq))
                                 :date (current-time)))
                 eshell-z-freq-dir-hash-table)
      (puthash key (cons key
                         (list :freq 1
                               :date (current-time)))
               eshell-z-freq-dir-hash-table))))

(add-hook 'eshell-post-command-hook #'eshell-z--add)

;; TODO: Try to process command option using esh-opt.el
(defun eshell/z (&rest args)
  "Jump around."
  (setq args (eshell-flatten-list args))
  (let ((paths (sort (hash-table-values eshell-z-freq-dir-hash-table)
                     (lambda (elt1 elt2)
                       (> (plist-get (cdr elt1) :freq)
                          (plist-get (cdr elt2) :freq))))))
    (if (null args)
        (completing-read "pattern " paths nil t)
      (let ((path (car args))
            (case-fold-search (eshell-under-windows-p)))
        (if (numberp path)
            (setq path (number-to-string path)))
        (if-let ((newdir
                  (caar (seq-filter
                         (lambda (elt)
                           (string-match path (car elt)))
                         paths))))
            (eshell/cd (list newdir)))))))

(provide 'eshell-z)
;;; eshell-z.el ends here
