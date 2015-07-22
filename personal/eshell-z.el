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
(require 'subr-x)

(defvar eshell-z-table (make-hash-table :test 'equal
                                        :size 100))

(defun eshell-z--add ()
  "Add entry."
  ;; $HOME isn't worth matching
  (unless (string= (expand-file-name default-directory)
                   (expand-file-name "~/"))
    (if-let ((key default-directory)
             (val (gethash key eshell-z-table)))
        (puthash key (cons key
                           (list :freq (1+ (plist-get (cdr val) :freq))
                                 :date (current-time)))
                 eshell-z-table)
      (puthash key (cons key
                         (list :freq 1
                               :date (current-time)))
               eshell-z-table))))

(add-hook 'eshell-post-command-hook #'eshell-z--add)

(defun eshell/z (&rest args)
  "Jump around."
  (setq args (eshell-flatten-list args))
  (let ((path (car args))
        (case-fold-search (eshell-under-windows-p)))
    (if (numberp path)
        (setq path (number-to-string path)))
    (if-let ((newdir
              (caar (seq-filter
                     (lambda (elt)
                       (string-match path (car elt)))
                     (sort (hash-table-values eshell-z-table)
                           (lambda (elt1 elt2)
                             (> (plist-get (cdr elt1) :freq)
                                (plist-get (cdr elt2) :freq))))))))
        (eshell/cd (list newdir)))))

(provide 'eshell-z)
;;; eshell-z.el ends here
