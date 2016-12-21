;;; chunyang-macros.el --- Macros                    -*- lexical-binding: t; -*-

;;; Commentary:

;; For learning Macros in Emacs Lisp.
;;
;; - msg
;; - for
;; - dotimes
;; - dolist
;; - aif/awhen
;; - if-let/when-let
;; - list-let

;;; Code:

(defmacro msg (&rest args)
  "A wrapper for `message': (msg a b) ≡ (message \"-> a=%s, b=%s\" a b)."
  (let ((format-string
         (concat "-> "
                 (mapconcat (lambda (s) (format "%s = %%s" s))
                            args ", "))))
    ;; (cons 'message (cons format-string args))
    ;; Or just
    `(message ,format-string ,@args)))

(defmacro for (var from init to final do &rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."
  (let ((tempvar (make-symbol "max")))
    `(let ((,var ,init)
           (,tempvar ,final))
       (while (<= ,var ,tempvar)
         ,@body
         (incf ,var)))))

;; (for i from 1 to 10 do (msg i))

(defmacro my-dotimes (count &rest body)
  (let ((idx (make-symbol "idx"))
        (max (make-symbol "max")))
    `(let ((,idx 0)
           (,max ,count))
       (while (< ,idx ,max)
         ,@body
         (incf ,idx)))))

;; (my-dotimes 3 (msg "hi"))

(defmacro my-dotimes2 (spec &rest body)
  "Almost the same as `dotimes'.

\(fn (VAR COUNT [RESULT]) BODY...)"
  (declare (indent 1))
  (let ((temp (make-symbol "limit"))
        (start 0)
        (end (nth 1 spec)))
    `(let ((,temp ,end)
           (,(car spec) ,start))
       (while (< ,(car spec) ,temp)
         ,@body
         (setq ,(car spec) (1+ ,(car spec))))
       ,(caddr spec))))

;; (dolist (i '(1 2 3)) (msg i))

(defmacro my-dolist (spec &rest body)
  "Almost the same as `dotimes'.

\(fn (VAR LIST) BODY...)"
  (declare (indent 1))
  (let ((temp (make-symbol "list")))
    `(let ((,temp ,(nth 1 spec)))
       (while ,temp
         (let ((,(car spec) (car ,temp)))
           ,@body)
         (setq ,temp (cdr ,temp))))))

;; (my-dolist (i '(1 2 4 3)) (msg i))

(defmacro my-aif (test-form then-form &rest else-forms)
  (declare (indent 2))
  `(let ((it ,test-form))
     (if it
         ,then-form
       ,@else-forms)))

;; (my-aif 42 (msg "then" it) (msg "else" it))

(defmacro my-awhen (test-form &rest then-forms)
  (declare (indent 1))
  `(let ((it ,test-form))
     (when it
       ,@then-forms)))

;; (my-awhen 42 (msg "succeed") (msg it))

(defmacro my-if-let (binding then &rest else)
  (declare (indent 2))
  `(let ((,(car binding) ,(cadr binding)))
     (if ,(car binding)
         ,then
       ,@else)))

;; (my-if-let (a 100)
;;     (msg a)
;;   (msg (incf a)))

(defmacro my-when-let (binding &rest body)
  (declare (indent 1))
  `(let ((,(car binding) ,(cadr binding)))
     (when ,(car binding)
       ,@body)))

;; (my-when-let (x 123)
;;   (incf x)
;;   (msg x))

;; TODO: How to get list evaluated like `seq-let'?
(defmacro list-let (vars list &rest body)
  (let* ((min (min (length vars) (length list)))
         (bindings (cl-loop repeat min
                            for var in vars
                            for val in list
                            collect (list var val))))
    `(let ,bindings
       ,@body)))

;; (list-let (a b) (1 2 3) (msg a b))

(provide 'chunyang-macros)
;;; chunyang-macros.el ends here
