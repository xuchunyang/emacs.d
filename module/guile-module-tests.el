;;; guile-module-tests.el --- Tests                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

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

;; gsl-module.c tests

;;; Code:

(require 'guile-module)
(require 'ert)

(ert-deftest guile-float-to-bytes ()
  (should (equal (guile-float-to-bytes 1.3) '(63 166 102 102)))
  (should (equal (guile-float-to-bytes 0.1) '(#x3d #xcc #xcc #xcd))))

(provide 'guile-module-tests)
;;; guile-module-tests.el ends here
