;;; chunyang-elisp.el --- Utilities for Emacs Lisp

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
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

(defvar chunyang-elisp-debug-buffer "*Debug ELisp Log*")

(defvar chunyang-elisp-debug t
  "If non-nil, write log message into `chunyang-elisp-debug-buffer' buffer.")

(defmacro aif (test-form then-form &rest else-forms)
  "Anaphoric if."
  (declare (indent 2))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro awhen (test &rest body)
  "Anaphoric when."
  (declare (indent 1))
  `(let ((it ,test))
     (when it ,@body)))


;;; TODO: Improve message & logging
;; Utility: logging
(defmacro m (&rest args)
  `(message-box "%s" (list ,@args)))

(defmacro mm (format-string &rest args)
  `(message-box ,format-string ,@args))

(defun chunyang-elisp-log (format-string &rest args)
  "Log message if `chunyang-elisp-debug-buffer' is non-nil.
Messages are written to  the `chunyang-elisp-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'.

Adapted from `helm-log'."
  (when chunyang-elisp-debug
    (let ((buffer (current-buffer)))
      (with-current-buffer (get-buffer-create chunyang-elisp-debug-buffer)
        (outline-mode)
        (buffer-disable-undo)
        (setq-local inhibit-read-only t)
        (goto-char (point-max))
        (insert (let ((tm (current-time)))
                  (format (concat (if (string-match "Start session" format-string)
                                      "* " "** ")
                                  "%s.%06d (%s:%d)\n %s\n")
                          (format-time-string "%H:%M:%S" tm)
                          (nth 2 tm)
                          buffer (line-number-at-pos)
                          (apply #'format (cons format-string args)))))))))

(defun view-debug-log-buffer ()
  "View the `*Debug ELisp Log*' buffer."
  (interactive)
  (display-buffer "*Debug ELisp Log*"))

(global-set-key (kbd "C-h g") #'view-debug-log-buffer)

(defalias 'my-log #'chunyang-elisp-log)
(defalias 'l #'chunyang-elisp-log)

(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;;; package
(defun open-package-melpa-page (package)
  "Adopt from `describe-package'."
  (interactive
   (let* ((guess (or (function-called-at-point)
                     (symbol-at-point))))
     (require 'finder-inf nil t)
     ;; Load the package list if necessary (but don't activate them).
     (unless package--initialized
       (package-initialize t))
     (let ((packages (append (mapcar 'car package-alist)
                             (mapcar 'car package-archive-contents)
                             (mapcar 'car package--builtins))))
       (unless (memq guess packages)
         (setq guess nil))
       (setq packages (mapcar 'symbol-name packages))
       (let ((val
              (completing-read (if guess
                                   (format "Describe package (default %s): "
                                           guess)
                                 "Describe package: ")
                               packages nil t nil nil (when guess
                                                        (symbol-name guess)))))
         (list (intern val))))))

  (if (not (or (package-desc-p package) (and package (symbolp package))))
      (message "No package specified")
    (browse-url (format "http://melpa.org/#/%s"
                        (symbol-name package)))))

;; This macro requires Emacs 25+
(define-advice describe-package-1 (:around (orig-fun &rest args) add-melpa-link)
  "Add package's MELPA link section."
  ;; Assuming all packages are on MELPA for convenience, though it's not true.
  (apply orig-fun args)
  (let* ((pkg (car args))
         (name (if (package-desc-p pkg)
                   (package-desc-name pkg)
                 (symbol-name pkg)))
         (melpa-link
          (format "http://melpa.org/#/%s" name)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "Summary:" nil t)
        (forward-line 1)
        (package--print-help-section "MELPA")
        (help-insert-xref-button melpa-link 'help-url melpa-link)
        (insert "\n")))))


;;; Ugly hack - Make `&' save recent value during C-x C-e, just like `*' in IELM

;; (with-eval-after-load "elisp-mode"
;;   (defvar & nil
;;     "Most recent value evaluated in *scratch*.")

;;   (defun eval-last-sexp (eval-last-sexp-arg-internal)
;;     "Re-define version by Chunyang Xu, for save recent value to `&'."
;;     (interactive "P")
;;     (if (null eval-expression-debug-on-error)
;;         (elisp--eval-last-sexp eval-last-sexp-arg-internal)
;;       (let ((value
;;              (let ((debug-on-error elisp--eval-last-sexp-fake-value))
;;                (cons (elisp--eval-last-sexp eval-last-sexp-arg-internal)
;;                      debug-on-error))))
;;         (unless (eq (cdr value) elisp--eval-last-sexp-fake-value)
;;           (setq debug-on-error (cdr value)))
;;         (prog1 (car value)
;;           (setq & (car value)))))))


;;; Another C-j for `lisp-interaction-mode'

;;*    Sample                                                           */
;;*---------------------------------------------------------------------*/
;; (setq x 23)
;;     ⇒ 23

;; (cl-incf x)
;;     ⇒ 24

;; (defun hi ()
;;   (message "hi"))
;;     ⇒ hi
;;*---------------------------------------------------------------------*/

(bind-key "C-j" #'my-eval-print-last-sexp lisp-interaction-mode-map)

(defun chunyang-disable-some-modes-in-scratch ()
  (dolist (mode '(aggressive-indent-mode ipretty-mode))
    (when (and (fboundp mode) mode)
      (funcall mode -1))))

(add-hook 'lisp-interaction-mode-hook #'chunyang-disable-some-modes-in-scratch)

(defun current-line-empty-p ()
  (string= "" (buffer-substring (line-beginning-position)
                                (line-end-position))))

(defun my-eval-print-last-sexp ()
  "Like `my-eval-print-last-sexp' but format result value a bit."
  (interactive)
  (require 'elisp-mode)
  (let ((standard-output (current-buffer)))
    (terpri)
    (let ((res
           (eval
            (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)))
      (unless (current-line-empty-p) (terpri))
      (princ "    ⇒ ")
      (princ (with-temp-buffer
               (elisp--eval-last-sexp-print-value res t)
               (buffer-string))))
    (unless (current-line-empty-p) (terpri))))

(define-minor-mode display-pos-mode
  "Display position in mode line mainly for testing and debugging."
  :global t
  :lighter (:eval (format " [%d]" (point))))


;; Display function's short docstring along side with args in eldoc
(define-advice elisp-get-fnsym-args-string (:around (orig-fun &rest r) append-func-doc)
  (concat
   (apply orig-fun r)
   (let* ((f (car r))
          (fdoc
           (and (fboundp f)
                (documentation f)))
          (fdoc-short
           (and (stringp fdoc)
                (substring fdoc 0 (string-match "\n" fdoc)))))
     (when (and (stringp fdoc-short)
                (not (string-blank-p fdoc-short)))
       ;; TODO: Face
       (concat "  |  " fdoc-short)))))

(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
