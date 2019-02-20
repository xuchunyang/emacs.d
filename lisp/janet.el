;;; janet.el --- Janet                               -*- lexical-binding: t; -*-

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

(require 'comint)

(define-derived-mode inferior-janet-mode comint-mode "Inferior Janet"
  "Major mode for Janet inferior process."
  ;; FIXME: the prompt is not displaying at all, not sure why
  ;; janet:0:>
  (setq comint-prompt-regexp (rx bol "janet:" (1+ num) ":>")))

;;;###autoload
(defun run-janet ()
  (interactive)
  (with-current-buffer (make-comint-in-buffer "Janet" "*Janet*" "janet" nil)
    (inferior-janet-mode)
    (display-buffer (current-buffer))))

(provide 'janet)
;;; janet.el ends here
