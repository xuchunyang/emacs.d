;;; number-word.el --- Convert Numbers to Words, and vice versa  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/emacs.d
;; Keywords: number
;; Created: 2017-03-02
;; Modified: 2018-01-09

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

(defun number-word-get-number (word)
  (cdr (assoc word '(("one"   . 1)
                     ("two"   . 2)
                     ("three" . 3)
                     ("four"  . 4)
                     ("five"  . 5)
                     ("six"   . 6)
                     ("seven" . 7)
                     ("eight" . 8)
                     ("nine"  . 9)
                     ("ten"   . 10)))))

(defun number-word-get-word (number)
  (car (rassq number '(("one"   . 1)
                       ("two"   . 2)
                       ("three" . 3)
                       ("four"  . 4)
                       ("five"  . 5)
                       ("six"   . 6)
                       ("seven" . 7)
                       ("eight" . 8)
                       ("nine"  . 9)
                       ("ten"   . 10)))))

(provide 'number-word)

;; Local Variables:
;; time-stamp-pattern: "20/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; number-word.el ends here
