;;; mark-align.el --- Align through Marking          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar mark-align-markers nil)

(defun mark-align-set-mark ()
  (interactive)
  (push (cons (point) (current-column)) mark-align-markers))

(defun mark-align-done ()
  (interactive)
  (when mark-align-markers
    (let ((max (apply #'max (mapcar #'cdr mark-align-markers)))
          (pts (sort mark-align-markers (lambda (a b)
                                          (> (car a) (car b))))))
      (dolist (pt pts)
        (goto-char (car pt))
        (let ((col (current-column)))
          (when (< col max)
            (insert (make-string (- max col) ?\s))))))
    (setq mark-align-markers nil))
  (mark-align-mode -1))

(defvar mark-align-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-SPC")   #'mark-align-set-mark)
    (define-key map (kbd "C-c C-c") #'mark-align-done)
    map))

(define-minor-mode mark-align-mode
  "Align through marking."
  :lighter " Mark-Align")


(provide 'mark-align)
;;; mark-align.el ends here
