(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; I already put the following in my emacs-init.org, but `package.el' requires
;; mentioning it in `user-init-file'.
(package-initialize)                    ; uncommented now because I need ELPA org

;;; Install org from Git, since I have trouble to access http://orgmode.org/elpa/
(add-to-list 'load-path "~/wip/org-mode/lisp")
;; Various org-mode extensions
(add-to-list 'load-path "~/wip/org-mode/contrib/lisp" :append)

;;; Install helm from Git
(add-to-list 'load-path "~/wip/async")
(add-to-list 'load-path "~/wip/helm")
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)

;; `org-babel-load-file' is autoloaded, no need require anything
(org-babel-load-file (locate-user-emacs-file "emacs-init.org"))
