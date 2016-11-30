;;; chunyang-package.el --- Hacks on package.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>

;;; Commentary:

;;

;;; Code:

(require 'subr-x)                       ; `if-let' and `when-let'

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

(provide 'chunyang-package)
;;; chunyang-package.el ends here
