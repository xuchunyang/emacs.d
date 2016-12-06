;;; chunyang-emacs-compatibility.el --- Emacs version compatibility  -*- lexical-binding: t; -*-

;;; Commentary:

;; Sometimes I need to use Emacs 24 with the Emacs configuration.

;;; Code:

;; Add in Emacs 25.1
(unless (fboundp 'define-advice)
  (defmacro define-advice (symbol args &rest body)
    "Define an advice and add it to function named SYMBOL.
See `advice-add' and `add-function' for explanation on the
arguments.  Note if NAME is nil the advice is anonymous;
otherwise it is named `SYMBOL@NAME'.

\(fn SYMBOL (WHERE LAMBDA-LIST &optional NAME DEPTH) &rest BODY)"
    (declare (indent 2) (doc-string 3) (debug (sexp sexp body)))
    (or (listp args) (signal 'wrong-type-argument (list 'listp args)))
    (or (<= 2 (length args) 4)
        (signal 'wrong-number-of-arguments (list 2 4 (length args))))
    (let* ((where         (nth 0 args))
           (lambda-list   (nth 1 args))
           (name          (nth 2 args))
           (depth         (nth 3 args))
           (props         (and depth `((depth . ,depth))))
           (advice (cond ((null name) `(lambda ,lambda-list ,@body))
                         ((or (stringp name) (symbolp name))
                          (intern (format "%s@%s" symbol name)))
                         (t (error "Unrecognized name spec `%S'" name)))))
      `(prog1 ,@(and (symbolp advice) `((defun ,advice ,lambda-list ,@body)))
         (advice-add ',symbol ,where #',advice ,@(and props `(',props)))))))

(provide 'chunyang-emacs-compatibility)
;;; chunyang-emacs-compatibility.el ends here
