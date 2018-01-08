;;; sml-eldoc.el --- sml-mode support for eldoc   -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang.me@gmail.com>

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

;; Extract type information from the REPL running in Emacs.
;;
;; To use, (add-hook 'sml-mode-hook 'sml-eldoc-turn-on)
;;
;;
;; TODO: Figure out datatype, type, exception etc, not just val-bindings
;; TODO: Understand signature, structure

;;; Code:

(defvar sml-eldoc-types nil)

(defun sml-eldoc-function ()
  (let ((word (current-word)))
    (and word
         (cdr (assq (intern word) sml-eldoc-types)))))

(defun sml-eldoc-output-filter (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    ;; NOTE: This is assuming we are using SML/NJ's REPL, not sure other REPLs
    ;; FIXME: Multiple lines ouput is not handled correctly.
    (while (re-search-forward "^val \\([^ ]*\\) = \\(.*\\)$" nil t)
      (let ((symbol (intern (match-string 1)))
            (type (match-string 2)))
        (setq sml-eldoc-types
              (cons (cons symbol type) (assq-delete-all symbol sml-eldoc-types)))))))

(defun sml-eldoc-collect ()
  (setq sml-eldoc-types nil)
  (add-hook 'comint-output-filter-functions 'sml-eldoc-output-filter nil t))

;;;###autoload
(defun sml-eldoc-turn-on ()
  (interactive)
  ;; FIXME: Test if this works on versions before Emacs 25
  (add-function :before-until (local 'eldoc-documentation-function)
                #'sml-eldoc-function)
  ;; Emacs 25 comes with `global-eldoc-mode'
  (unless (and (fboundp 'global-eldoc-mode) global-eldoc-mode)
    (eldoc-mode))
  (add-hook 'sml-prog-proc-comint-mode-hook #'sml-eldoc-collect))

(provide 'sml-eldoc)
;;; sml-eldoc.el ends here
