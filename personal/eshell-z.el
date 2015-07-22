;;; eshell-z.el --- jump around

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Keywords: convenience
;; Version: 0.1
;; URL: https://github.com/xuchunyang/emacs.d

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

(provide 'eshell-z)
;;; eshell-z.el ends here
