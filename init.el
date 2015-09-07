(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; I already put the following in my emacs-init.org, but `package.el' requires
;; mentioning it in `user-init-file'.
(package-initialize)                    ; uncommented now because I need ELPA org

;; `org-babel-load-file' is autoloaded, no need require anything
(org-babel-load-file (locate-user-emacs-file "emacs-init.org"))
