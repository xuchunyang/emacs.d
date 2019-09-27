;;; chunyang-package.el --- Hacks on package.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2016, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

;;; Commentary:

;;

;;; Code:

(require 'subr-x)                       ; `if-let' and `when-let'
(require 'package)

(defun chunyang-package-menu-hack-tabulated-list-format ()
  "Hack `tabulated-list-format' in the Package Menu."
  ;; The default is 18
  (setf (cadr (aref tabulated-list-format 0)) 24))

(defun chunyang-package-homepage (pkg)
  "Return homepage of package PKG (a symbol), or nil if none."
  (when-let ((desc (or
                    (cadr (assq pkg package-alist))
                    (if-let ((built-in (assq pkg package--builtins)))
                        (package--from-builtin built-in)
                      (cadr (assq pkg package-archive-contents)))))
             (extras (package-desc-extras desc))
             (homepage (cdr (assoc :url extras))))
    homepage))

;; For other way to reuse interactive-form, see
;; [[https://emacs.stackexchange.com/questions/28518/reuse-other-commands-interactive-form][elisp - Reuse other command's "interactive" form - Emacs Stack Exchange]]
(defun package-open-homepage (pkg)
  (when-let ((homepage (chunyang-package-homepage pkg)))
    (browse-url homepage)))
(put 'package-open-homepage 'interactive-form (interactive-form 'describe-package))

(defun chunyang-package-issue (pkg)
  "Return issue package of package PKG (a symbol), or nil if none.
Only projects on GitHub are supported."
  (when-let ((homepage (chunyang-package-homepage pkg))
             (issue (and
                     (string-match-p "^https?://github\\.com/"
                                     (chunyang-package-homepage 'pkg-info))
                     (concat homepage
                             (unless (string-suffix-p "/" homepage) "/")
                             "issues/"))))
    issue))

;; https://github.com/isaacs/github/issues/new?title=foo&body=bar
(defun chunyang-package-new-issue (pkg)
  "Create a new issue for package PKG.
Only projects on GitHub are supported."
  ;; http://emacs.stackexchange.com/a/28520/3889
  (interactive
   (eval (cadr (interactive-form 'describe-package))))
  (when-let ((issue (chunyang-package-issue pkg))
             (url (format "%s/new?title=foo&body=%s"
                          issue
                          (url-hexify-string
                           (concat "Emacs: " emacs-version "\n"
                                   (format "%s: %s\n" pkg (pkg-info-package-version pkg)))))))
    (browse-url url)))

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



;;; Emacs -Q

(require 'epl)
(require 'pkg-info)                     ; `pkg-info--read-package'
(require 'seq)

(defun chunyang-package-reqs (pkg)
  (let ((reqs (seq-remove
               #'epl-built-in-p
               (mapcar #'epl-requirement-name
                       (epl-package-requirements
                        (car (epl-find-installed-packages pkg)))))))
    (dolist (req reqs)
      (setq reqs (seq-uniq (append reqs (chunyang-package-reqs req)) #'eq)))
    reqs))

(defun chunyang-package-load-path (pkg)
  (mapcar
   (lambda (pkg) (epl-package-directory (car (epl-find-installed-packages pkg))))
   (cons pkg (chunyang-package-reqs pkg))))

(defun chunyang-package-emacs-Q-command (pkg)
  (interactive (list (intern (pkg-info--read-package))))
  (let ((cmd (mapconcat
              #'shell-quote-argument
              `(,(concat invocation-directory invocation-name)
                "-Q" "-nw"
                ,@(mapcan
                   (lambda (dir) (list "-L" dir))
                   (chunyang-package-load-path pkg)))
              " ")))
    (message "Copied: %s" cmd)
    (kill-new cmd)))

(provide 'chunyang-package)
;;; chunyang-package.el ends here
