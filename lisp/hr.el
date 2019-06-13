;;; hr.el --- <hr> for Emacs  -*- lexical-binding: t; -*-

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

;; Inspired by https://github.com/LuRsT/hr

;;; Code:

(defun hr (s width)
  "Repeat S to WIDTH."
  (while (< (length s) width)
    (setq s (concat s s)))
  (substring s 0 width))

(defun eshell/hr (&optional s)
  "Repeat S to the width of the window."
  (eshell-printn (hr (or s "#") (window-width)))
  nil)

(defun hr-insert (s)
  "Repeatedly insert S till the width of the window."
  (interactive
   (list
    (cond
     ((use-region-p) (buffer-substring-no-properties
                      (region-beginning) (region-end)))
     (t (string (preceding-char))))))
  (insert (hr s (- (window-width) (current-column)))))

(provide 'hr)
;;; hr.el ends here
