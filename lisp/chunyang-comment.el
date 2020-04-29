;;; chunyang-comment.el --- Insert fancy comments (section/box)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: comment

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

;; See also Emacs's builtin Comment system in `newcomment.el' and
;; [[https://github.com/lewang/rebox2][rebox2]].

;;; Code:

;; (defvar chunyang-comment-templates
;;   '((fill-column
;;      ("??*------------*/"
;;       "??*    Section */"
;;       "??*------------*/"))
;;     "??? *** Section ***"
;;     "/*** Section ***/"))

;;*-----------------------------------------------------------------*/
;;*    User commands...                                             */
;;*-----------------------------------------------------------------*/

;;;###autoload
(defun chunyang-comment-section (start end)
  (interactive "*r")
  (cond ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode
                                            racket-mode))
         (let* ((c
                 (substring comment-start 0 1))
                (template
                 (concat
                  c c "*" (make-string (- fill-column 5) ?-) "*/" "\n"
                  c c "*" "%s" "*/" "\n"
                  c c "*" (make-string (- fill-column 5) ?-)  "*/" "\n"))
                (comment
                 (buffer-substring start end))
                (comment-with-padding
                 (format (concat "%-" (number-to-string (- fill-column 5)) "s")
                         (concat "    " comment))))
           (delete-region start end)
           (insert (format template comment-with-padding))))
        ((memq major-mode '(c-mode))
         (let ((comment
                (buffer-substring start end))
               (template
                "/*** %s ***/"))
           (delete-region start end)
           (insert (format template comment))))))

;; TODO: Provide a helm command for previewing
;; TODO: Support more fancy comment boxes, such as

;;; **********************************************************************
;;; 			Buffer Local Functions
;;; **********************************************************************

;;; *** Buffer Local Definitions ***

;; ------------------
;; Operator terminals
;; ------------------

;;;============================================================================
;;;@@ print_stmt
;;;============================================================================

;;;---------------------------------------------------------------------
;;; Variables

;;;
;;; Generic index gathering function.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main functions for this package!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; ------------------------------------------------------------
;;;; User customization variables.
;;;; ------------------------------------------------------------

;;;;; ****************** HACKS TO THE REST OF EMACS ******************* ;;;;;

;;; Creating labels ---------------------------------------------------------

;;;;;;;;;;;;;;;;;
;; comment-box ;;
;;;;;;;;;;;;;;;;;



;; TODO: Write this style
;; # ----------------------------
;; # Subroutine to echo the usage
;; # ----------------------------

(defun chunyang-insert-comment-section (name)
  ;; TODO: Preview the result while typing!!!
  (interactive "sComment section: ")
  (unless (eq major-mode 'sh-mode)
    (user-error "FIXME: Unsupported Major Mode"))
  ;; #-- [ hello ] --#
  (let* ((len (- (/ (- fill-column (length name)) 2) 4))
         (padding (make-string len ?-)))
    (insert (format "#%s [ %s ] %s#"
                    padding
                    name
                    padding))))

(provide 'chunyang-comment)
;;; chunyang-comment.el ends here
