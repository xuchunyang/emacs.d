;; I already put the following in my emacs-init.org, but `package.el' requires
;; mentioning it in `user-init-file'.
(package-initialize)                    ; uncommented now because I need ELPA org
(setq package-enable-at-startup t)      ; force to run `package-initialize'
                                        ; again (for Emacs commit 066b26d)

;; `org-babel-load-file' is autoloaded, no need require anything
(org-babel-load-file (locate-user-emacs-file "emacs-init.org"))
