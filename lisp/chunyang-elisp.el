;;; chunyang-elisp.el --- Utilities for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017, 2018  Xu Chunyang

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for Emacs Lisp

;;; Code:

(defun chunyang-eval-last-sexp-in-next-window ()
  (interactive)
  (pcase (window-list)
    (`(,_ ,next-window)
     (let ((sexp (sexp-at-point)))
       (with-current-buffer (window-buffer next-window)
         (eval-expression sexp))))
    (_ (user-error "Should be only two window"))))



(defvar chunyang-eval-print-last-sexp-prefix (car '(";; => "
                                                    "     => "
                                                    "     ⇒ ")))

(defun chunyang-eval-print-last-sexp (&optional eval-last-sexp-arg-internal)
  "Adapted from `eval-print-last-sexp'."
  (interactive "P")
  (let ((standard-output (current-buffer))
        (error-descriptor nil))
    (terpri)
    (let* ((p0 (point))
           (p1 (condition-case err
                   (progn (eval-last-sexp (or eval-last-sexp-arg-internal t))
                          (point))
                 (error (setq error-descriptor err))))
           (prompt (if error-descriptor
                       "error-> "       ; error→
                     chunyang-eval-print-last-sexp-prefix)))
      (if (not error-descriptor)
          (progn (goto-char p0)
                 (princ prompt)
                 (goto-char (+ p1 (length prompt))))
        (princ prompt)
        (princ (error-message-string error-descriptor))))
    (terpri)
    (when error-descriptor
      ;; Re-throw the error signal
      (signal (car error-descriptor) (cdr error-descriptor)))))

(defun chunyang-eval-print-last-sexp-use-comment ()
  (interactive)
  (let ((standard-output (current-buffer)))
    (let ((res (eval (eval-sexp-add-defvars (elisp--preceding-sexp))
                     lexical-binding)))
      (princ ";=> ")
      (princ res)
      ;; Indent comment
      (comment-dwim nil))))

(defun chunyang-current-line-empty-p ()
  (string= "" (buffer-substring (line-beginning-position)
                                (line-end-position))))

(defun chunyang-macroexpand-print-last-sexp ()
  (interactive)
  (let ((standard-output (current-buffer)))
    (terpri)
    (let ((res (macroexpand (elisp--preceding-sexp))))
      (princ "     ↦ ")
      (princ res)
      (unless (chunyang-current-line-empty-p) (terpri)))))



(define-minor-mode chunyang-display-mark-and-pos-mode
  "Display [mark, point] in mode line for testing and debugging."
  :global t
  :lighter (:eval (format " [%s, %d]" (mark) (point))))

(define-minor-mode chunyang-display-window-mode
  "Display window in the mode line."
  :global t
  :lighter (:eval
            ;; #<window 229 on *scratch*>
            ;; #<window 229>
            (let ((str (prin1-to-string (selected-window))))
              (and (string-match "[0-9]+" str)
                   (format " #<window %s>" (match-string 0 str))))))

;; Inspired by https://with-emacs.com/images/show_paren.gif
(define-minor-mode display-command-mode
  "Display last command and key on the mode line."
  :lighter (:eval (display-command-mode--string))
  ;; FIXME Define `global-display-command-mode' instead  
  :global t)

(defun display-command-mode--string ()
  (let* ((keys (recent-keys 'include-cmds))
         (i (1- (length keys)))
         (read (lambda ()
                 (let ((cmd (cdr (aref keys i)))
                       key)
                   (setq i (1- i))
                   (while (and (>= i 0) (pcase (aref keys i)
                                          (`(nil . ,_) nil)
                                          (_ t)))
                     (push (aref keys i) key)
                     (setq i (1- i)))
                   (list :cmd cmd :key (key-description (vconcat key)))))))
    (let ((last-key (funcall read))
          (times 1))
      (while (equal last-key (funcall read))
        (cl-incf times))
      (format " %s %s%s"
              (plist-get last-key :key)
              (plist-get last-key :cmd)
              (if (> times 1)
                  (format " x%d" times)
                "")))))

;; Byte-code, disassemble

(define-advice disassemble (:after (object &rest _) revert-buffer)
  "Make `g' (`revert-buffer') works for *Disassemble* buffer."
  (with-current-buffer "*Disassemble*"
    (unless (eq revert-buffer-function 'disassemble--revert-buffer)
      (setq-local revert-buffer-function
                  (defun disassemble--revert-buffer (_ignore-auto _noconfirm)
                    (disassemble object))))))


;;; Eval

(defun chunyang-eval-region-as-key (beg end)
  "Eval region as Emacs key."
  ;; The region can be Emacs key:
  ;;
  ;; [?\C-u ?\C-n]
  ;; "\C-u \C-n"
  ;; C-u M-x emacs-version RET
  (interactive "r")
  (let* ((sel (buffer-substring-no-properties beg end))
         (key (if (member (aref sel 0) '(?\" ?\[) )
                  (read sel)
                (kbd sel))))
    (deactivate-mark)
    (command-execute key)))

(defun chunyang-eval-on-region (beg end)
  "Eval a sexp on the region, local var `beg', `end' and `text' can be used.'"
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (eval (read--expression "Eval on region (local: beg, end, text): ")
          ;; hmm, really necessary? I still don't know very well on
          ;; Emacs Lisp scope/environment
          (list (cons 'beg beg)
                (cons 'end end)
                (cons 'text text)))))


;;; Key

(defun chunyang-insert-command (key)
  "Insert the command name on KEY."
  (interactive "kInsert command of Key: ")
  (insert (symbol-name (key-binding key))))

(defun chunyang-insert-key (key)
  "Insert the kbd representation of KEY."
  (interactive "kInsert kbd form of key: ")
  (insert (prin1-to-string `(kbd ,(key-description key)))))

(make-obsolete 'chunyang-insert-key-by-key 'chunyang-insert-key "2018-04-08")

(defun chunyang-insert-key-by-key (&optional ask-for-kind)
  (interactive "P")
  (let ((kind (if ask-for-kind
                  (let ((choices '(("Human-readable String" . human)
                                   ("Emacs Lisp String"     . string)
                                   ("Emacs Lisp Vector"     . vector))))
                    (cdr (assoc (completing-read "Insert key as: " choices nil t)
                                choices)))
                'human))
        (key (read-key-sequence "Key: ")))
    (cl-case kind
      (human  (insert (key-description key)))
      (string (insert key))
      (vector (insert (format "%s" (read-kbd-macro key t)))))))

(defvar chunyang-format-command-on-key--formats
  '(("Markdown     `C-n` (`next-line`)"          . markdown)
    ("Markdown kbd <kbd>C-n</kbd> (`next-line`)" . markdown-kbd)
    ("Org          ~C-n~ (~next-line~)"          . org)
    ("Emacs Lisp   C-n (`next-line')"            . emacs-lisp)
    ("Default      'C-n' ('next-line')"          . default)))

(defun chunyang-format-command-on-key--default-format ()
  (cl-case major-mode
    (emacs-lisp-mode          'emacs-lisp)
    (org-mode                 'org)
    ((markdown-mode gfm-mode) 'markdown)
    (t                        'default)))

(defun chunyang-format-command-on-key (buffer key &optional format)
  "Format command on KEY in BUFFER in FORMAT.
With prefix arg, ask the format to use, otherwise, the format is
picked according to the major-mode."
  (interactive
   (let* ((buf (read-buffer "Buffer: " (current-buffer)))
          (key (with-current-buffer buf
                 (read-key-sequence (format "Run Key in %S: " buf))))
          (default-format
            (chunyang-format-command-on-key--default-format))
          (default-format-desc
            (car (rassq default-format chunyang-format-command-on-key--formats)))
          (format (if current-prefix-arg
                      (cdr (assoc
                            (completing-read "Format: "
                                             chunyang-format-command-on-key--formats
                                             nil t nil nil
                                             default-format-desc)
                            chunyang-format-command-on-key--formats))
                    default-format)))
     (list buf key format)))
  ;; In case calling from Lisp and FORMAT is omitted
  (setq format (or format (chunyang-format-command-on-key--default-format)))
  (let (key-name cmd result)
    (with-current-buffer (or buffer (current-buffer))
      (setq key-name (key-description key)
            cmd (key-binding key)))
    (setq result
          (cl-case format
            (org
             (format "~%s~ (~%s~)" key-name cmd))
            (markdown
             (format "`%s` (`%s`)" key-name cmd))
            (markdown-kbd
             (format "<kbd>%s</kbd> (`%s`)" key-name cmd))
            (emacs-lisp
             (format "%s (`%s')" key-name cmd))
            (default (format "'%s' ('%s')" key-name cmd))))
    (kill-new result)
    (message "Killed: %s" result)
    (insert result)))

;; Quote C-h k in the format of [[notmuch:id:47D77E66-DE07-4D57-AC23-921ADB4537B5@ucsd.edu][Email from Charles Berry: Re: {O} {Feature Request} Prov]]
(defun chunyang-format-help-on-key (buffer)
  "Quote the contents of *Help* after calling C-h k."
  (interactive "bDescribe key in buffer")
  (let* ((args (eval (cadr (interactive-form #'describe-key))))
         (key (car args))
         help-message result)
    (save-window-excursion
      (with-current-buffer buffer
        (apply #'describe-key args))
      (with-current-buffer "*Help*"
        (setq help-message
              (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert help-message)
        (goto-char (point-min))
        (while (not (eobp))
          (goto-char (line-beginning-position))
          (insert "! ")
          (forward-line))
        (goto-char (point-min))
        (insert (format ",----[ C-h k %s ]\n" (key-description key)))
        (goto-char (point-max))
        (insert "`----\n")
        (setq result (buffer-string)))
      (insert result))))

;; According to my test ('M-x trace-function undefined'), when I type
;; some undefined key, the command `undefined' will be called.
;; (define-advice undefined (:after () do-something-when-key-binding-not-defined)
;;   "Serve as common-not-found hook.

;; At first, I want 'Did you mena xxx?', but I don't know how to implement."
;;   (message "%s is undefined"
;;            (propertize (key-description (this-single-command-keys))
;;                        'face 'error))
;;   )


;;; Hash Table

(defun chunyang-hash-table-to-alist (hash-table)
  (let ((alist '()))
    (maphash (lambda (k v) (push (cons k v) alist)) hash-table)
    (nreverse alist)))

;; (let ((ht (make-hash-table :test 'equal)))
;;   (puthash :one 1 ht)
;;   (puthash :two 2 ht)
;;   (chunyang-hash-table-to-alist ht))
;;      ⇒ ((:one . 1) (:two . 2))

(defun chunyang-make-hash-table-from-alist (alist &rest keyword-args)
  "Create and return a new hash table from ALIST.

KEYWORD-ARGS is same as `make-hash-table'."
  (cl-loop with hash-table = (apply 'make-hash-table keyword-args)
           for (k . v) in alist
           do (puthash k v hash-table)
           finally return hash-table))

;; (chunyang-hash-table-to-alist
;;  (chunyang-make-hash-table-from-alist
;;   '(("one" . 1) ("two" . 2)) :test 'equal))
;;      ⇒ (("one" . 1) ("two" . 2))

(defun chunyang-plist-to-alist (plist)
  (cl-loop for x on plist by #'cddr
           collect (cons (car x) (cadr x))))

;; (chunyang-plist-to-alist '(:one 1 :two 2))
;;      ⇒ ((:one . 1) (:two . 2))

(defun chunyang-alist-to-plist (alist)
  (cl-loop for (k . v) in alist
           append (list k v)))

;; (chunyang-alist-to-plist '((:one . 1) (:two . 2)))
;;      ⇒ (:one 1 :two 2)


;;; Binary format in C-x C-e
(declare-function chunyang-format-as-binary "chunyang-misc")

(define-advice eval-expression-print-format (:around (old-fun value) binary)
  "Show (Binary, Octal, Hex, Char) for numbers."
  (let ((rtv (funcall old-fun value)))
    (if rtv
        (replace-regexp-in-string
         "(#o"
         (format "(%s, #o" (chunyang-format-as-binary value))
         rtv))))


;;; Group & split setq

(defun chunyang-split-one-setq-form (beg end)
  (interactive "r")
  (goto-char beg)
  (let ((expr
         (save-excursion
           (read (current-buffer)))))
    (delete-region beg end)
    (insert
     (mapconcat
      ;; or try `pp-to-string'
      #'prin1-to-string
      (mapcar
       (lambda (elt)
         (cons 'setq elt))
       (seq-partition (cdr expr) 2))
      "\n"))))

(defun chunyang-group-multi-setq-forms (beg end)
  (interactive "r")
  (goto-char beg)
  (let (exprs)
    (save-excursion
      (while (< (point) end)
        (push (read (current-buffer)) exprs)))
    (setq exprs (nreverse exprs))
    (delete-region beg end)
    (insert (prin1-to-string (cons 'setq (seq-mapcat #'cdr exprs))))))

(defun chunyang-toggle-setq-form (beg end)
  (interactive "r")
  (goto-char beg)
  (let ((expr (save-excursion
                (read (current-buffer)))))
    (if (> (length expr) 3)
        (chunyang-split-one-setq-form beg end)
      (chunyang-group-multi-setq-forms beg end))))


;; (info "(elisp) Box Diagrams")

;; (insert (chunyang-box-diagram '(rose violet buttercup)))
;;  --- ---      --- ---       --- ---         
;; |   |   |--> |   |   |-->  |   |   |--> nil 
;;  --- ---      --- ---       --- ---         
;;   |            |             |              
;;   |            |             |              
;;    --> rose     --> violet    --> buttercup nil

;; (with-current-buffer (get-buffer-create "*test*")
;;   (erase-buffer)
;;   (insert (chunyang-box-diagram (number-sequence 1 10)))
;;   (goto-char (point-min))
;;   (display-buffer (current-buffer)))

;; TODO: Try the second manner
;; TODO: How to draw ((pine needles) oak maple)?

;;;###autoload
(defun chunyang-box-diagram (list)
  "Draw LIST as box diagrams, see Info node `(elisp) Box Diagrams'."
  (let (boxes)
    ;; one element one box
    (while list
      (push (format "\
 --- ---
|   |   |--> %s
 --- ---
  |
  |
   --> %s " (if (cdr list) "" "nil") (car list)) boxes)
      (setq list (cdr list)))
    (setq boxes (nreverse boxes))
    ;; make box rectangle
    (setq boxes
          (mapcar
           (lambda (box)
             (let* ((lines (split-string box "\n"))
                    (maxlen (apply #'max (mapcar #'length lines))))
               (mapconcat
                (lambda (line)
                  (format (format "%%-%ds" maxlen) line))
                lines "\n")))
           boxes))
    ;; concat the boxes
    (setq boxes (mapcar (lambda (box) (split-string box "\n")) boxes))
    (cl-loop repeat (length (car boxes))
             for i from 0
             collect (apply #'concat (mapcar (lambda (box) (nth i box)) boxes)) into lines
             finally return (mapconcat #'identity lines "\n"))))

(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
