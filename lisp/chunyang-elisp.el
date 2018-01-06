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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities for Emacs Lisp

;;; Code:


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
                     "     => "         ; ⇒
                     )))
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
      (princ "     ≡ ")
      (princ res)
      (unless (chunyang-current-line-empty-p) (terpri)))))



(define-minor-mode chunyang-display-mark-and-pos-mode
  "Display [mark, point] in mode line for testing and debugging."
  :global t
  :lighter (:eval (format " [%s, %d]" (mark) (point))))


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

(defun chunyang-insert-command-name-by-key ()
  (interactive)
  (insert (format "%s" (key-binding (read-key-sequence "Key: ")))))

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
  (loop with hash-table = (apply 'make-hash-table keyword-args)
        for (k . v) in alist
        do (puthash k v hash-table)
        finally return hash-table))

;; (chunyang-hash-table-to-alist
;;  (chunyang-make-hash-table-from-alist
;;   '(("one" . 1) ("two" . 2)) :test 'equal))
;;      ⇒ (("one" . 1) ("two" . 2))

(defun chunyang-plist-to-alist (plist)
  (loop for x on plist by #'cddr
        collect (cons (car x) (cadr x))))

;; (chunyang-plist-to-alist '(:one 1 :two 2))
;;      ⇒ ((:one . 1) (:two . 2))

(defun chunyang-alist-to-plist (alist)
  (loop for (k . v) in alist
        append (list k v)))

;; (chunyang-alist-to-plist '((:one . 1) (:two . 2)))
;;      ⇒ (:one 1 :two 2)


;;; Binary format in C-x C-e

(define-advice eval-expression-print-format (:around (old-fun value) binary)
  "Show (Binary, Octal, Hex, Char) for numbers."
  (let ((rtv (funcall old-fun value)))
    (if rtv
        (replace-regexp-in-string
         "(#o"
         (format "(%s, #o" (chunyang-format-as-binary value))
         rtv))))

(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
