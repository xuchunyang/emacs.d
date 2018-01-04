;;; mark-align.el --- Align through Marking          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Xu Chunyang

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar-local mark-align-markers  nil)
(defvar-local mark-align-overlays nil)

(defun mark-align-set-mark-or-unset ()
  (interactive)
  (let ((overlays (overlays-at (point)))
        found)
    (while overlays
      (let ((ov (car overlays)))
        (when (overlay-get ov 'mark-align)
          (setq found ov
                overlays nil))))
    (if found
        (progn (setq mark-align-markers
                     (cl-remove-if (lambda (pos-col)
                                     (= (car pos-col)
                                        (overlay-start found)))
                                   mark-align-markers)
                     mark-align-overlays
                     (delete found mark-align-overlays))
               (delete-overlay found))
      (push (cons (point) (current-column)) mark-align-markers)
      (let ((ov (make-overlay (point) (1+ (point)))))
        (overlay-put ov 'mark-align t)
        (overlay-put ov 'face 'underline)
        (push ov mark-align-overlays)))))

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
    (setq mark-align-markers nil)
    (mapc #'delete-overlay mark-align-overlays)
    (setq mark-align-overlays nil))
  (mark-align-mode -1))

(defvar mark-align-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-SPC")   #'mark-align-set-mark-or-unset)
    (define-key map (kbd "C-c C-c") #'mark-align-done)
    map))

(define-minor-mode mark-align-mode
  "Align through marking."
  :lighter " Mark-Align"
  (and mark-align-mode (mark-align-set-mark-or-unset)))

(provide 'mark-align)
;;; mark-align.el ends here
