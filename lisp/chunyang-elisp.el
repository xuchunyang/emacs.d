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


;; TODO: Log


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

(defun describe-package--add-melpa-link (pkg)
  (let* ((desc (if (package-desc-p pkg)
                   pkg
                 (cadr (assq pkg package-archive-contents))))
         (name (if desc (package-desc-name desc) pkg))
         (archive (if desc (package-desc-archive desc)))
         (melpa-link (format "https://melpa.org/#/%s" name)))
    (when (equal archive "melpa")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "Summary:" nil t)
          (forward-line 1)
          (package--print-help-section "MELPA")
          (help-insert-xref-button melpa-link 'help-url melpa-link)
          (insert "\n"))))))

(when (>= emacs-major-version 25)
  (advice-add 'describe-package-1 :after #'describe-package--add-melpa-link))


;;; Upgrade packages without 'M-x package-list-packages'
(defun package--outdated-packages ()
  "Return a list of names of packages outdated."
  (cl-loop for p in (mapcar #'car package-alist)
           with get-version = (lambda (pkg where)
                                (let ((desc (cadr (assq pkg where))))
                                  (and desc (package-desc-version desc))))
           for v1 = (funcall get-version p package-alist)
           for v2 = (funcall get-version p package-archive-contents)
           when (and v1 v2 (version-list-< v1 v2))
           collect p))

;; TODO: Finish this (NOTE: package-utils provides this kind of function)
(defun package-upgrade ()
  (interactive)
  (mapc (lambda (p)
          (let ((mode-line-process '(t (format "Upgrading %s..." p))))
            (force-mode-line-update)
            (package-install (cadr (assq p package-archive-contents)))
            (package-delete (cadr (assq p package-alist)))))
        (package--outdated-packages)))

;; (when (>= emacs-major-version 25)
;;   (define-advice package-install (:around (orig-fun &rest args) update-pkg-database-if-needed)
;;     (if (called-interactively-p 'any)
;;         (condition-case err
;;             (apply orig-fun args)
;;           (file-error
;;            (message "%s" (error-message-string err))
;;            (sit-for 1)
;;            (message "Refreshing package database...")
;;            (package-refresh-contents)
;;            (apply orig-fun args)))
;;       (apply orig-fun args))))

;; This ugly hack is to reuse the interactive from of `package-install'.
;; (eval
;;  `(defun package-install-maybe-refresh (pkg &optional dont-select)
;;     "Like `package-install' but call `package-refresh-contents' once if PKG is not found."
;;     ,(interactive-form 'package-install)
;;     (condition-case err
;;         (package-install pkg dont-select)
;;       (file-error
;;        (message "%s" (error-message-string err))
;;        (sit-for .5)
;;        (package-refresh-contents)
;;        (package-install pkg dont-select)))))


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

(unless (version< emacs-version "25")
  (bind-key "C-j" #'my-eval-print-last-sexp lisp-interaction-mode-map))

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
      (princ "     => ")
      (princ (with-temp-buffer
               (elisp--eval-last-sexp-print-value res t)
               (buffer-string))))
    (unless (current-line-empty-p) (terpri))))


(define-minor-mode display-pos-mode
  "Display position in mode line mainly for testing and debugging."
  :global t
  :lighter (:eval (format " [%d]" (point))))


;; Display function's short docstring along side with args in eldoc
(when (>= emacs-major-version 25)
  (define-advice elisp-get-fnsym-args-string (:around (orig-fun &rest r) append-func-doc)
    (concat
     (apply orig-fun r)
     (let* ((f (car r))
            (fdoc
             (and (fboundp f)
                  (documentation f 'raw)))
            (fdoc-one-line
             (and fdoc
                  (substring fdoc 0 (string-match "\n" fdoc)))))
       (when (and fdoc-one-line
                  (not (string= "" fdoc-one-line)))
         (concat "  |  " (propertize fdoc-one-line 'face 'italic)))))))


;; Misc
(defun describe-command (command)
  (interactive
   (let* ((fn (function-called-at-point))
          (cmd (and (commandp fn) fn))
          val)
     (setq val (completing-read (if cmd
                                    (format "Describe command (default %s): " cmd)
                                  "Describe command: ")
                                obarray 'commandp t nil nil
                                (and cmd (symbol-name cmd))))
     (list (if (equal val "")
               cmd (intern val)))))
  (describe-function command))


(provide 'chunyang-elisp)

;;; chunyang-elisp.el ends here
