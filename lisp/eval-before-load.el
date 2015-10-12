;;; eval-before-load.el --- Like `eval-after-load' but s/after/before  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>

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

;; NOTICE
;;
;; Unlike `eval-after-load', currently `eval-before-load' works only for
;; feature, that means, the first argument FEATURE of `eval-before-load' must be
;; a feature, i.e., a symbol, such as 'helm-config.
;;
;; As you can see, `eval-before-load' is better to renamed as
;; `eval-before-require', but for its expansibility and I like shorter name,
;; I didn't do it.
;;
;; To learn what a feature is, see (info "(elisp) Named Features")

;; TODO: Find out some practical use cases, if any
;; TODO: Integrate to `use-package', to serve as a keyword, if I want

;;; Code:
(defvar before-load-alist nil)

(defun require--eval-before-load (orig-fun &rest args)
  "Evaluate something right before `require'."
  (let ((feature (car args)))
    (when (not (featurep feature))
      (let ((func-list (assoc-default feature before-load-alist)))
        ;; Warning Error in `*Warning*' buffer if any
        (condition-case err
            (mapc #'funcall func-list)
          (error
           (ignore
            (display-warning 'eval-before-load
                             (format "%s: %s"
                                     feature (error-message-string err))
                             :error)))))))
  (apply orig-fun args))

(advice-add 'require :around #'require--eval-before-load)

;;;###autoload
(defun eval-before-load (feature form)
  "Arrange that if FEATURE'll be loaded, FROM'll be run immediately beforehand.
If FEATURE is already loaded, evaluate FORM right now.
FEATURE is a symbol like 'helm-config."
  ;; Add this FORM into before-load-alist (regardless of whether we'll be
  ;; evaluating it now).
  (let* ((feature (if (stringp feature) (intern feature)
                    feature))
         (elt (assoc feature before-load-alist))
         (func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding))))
    (unless elt
      (setq elt (list feature))
      (push elt before-load-alist))
    ;; Is feature already loaded?
    (prog1 (if (featurep feature)
               (funcall func))
      (unless (member func (cdr elt))
        (nconc elt (list func))))))
(put 'eval-before-load 'lisp-indent-function 1)

;;;###autoload
(defmacro with-eval-before-load (feature &rest body)
  "Execute BODY right before FEATURE will be loaded."
  (declare (indent 1))
  `(eval-before-load ,feature (lambda () ,@body)))

(provide 'eval-before-load)
;;; eval-before-load.el ends here
