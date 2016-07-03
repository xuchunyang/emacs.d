(setq org-emphasis-regexp-components
      ;; markup 记号前后允许中文
      (list (concat " \t('\"{"            "[:nonascii:]")
            (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
            " \t\r\n,\"'"
            "."
            1))

;; (with-eval-after-load 'org
;;   (setq org-match-substring-regexp
;;         (concat
;;          ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
;;          "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
;;          "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
;;          "\\|"
;;          "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
;;          "\\|"
;;          "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)")))

(config org
  ;; Use org from git repo
  ;; (add-to-list 'load-path "~/Projects/org-mode/lisp")
  ;; (add-to-list 'load-path "~/Projects/org-mode/contrib/lisp")
  ;; (add-to-list 'Info-directory-list "~/Projects/org-mode/doc")

  ;; Prevent demoting heading also shifting text inside sections
  ;; (setq org-adapt-indentation nil)

  ;; Global keys
  (bind-keys :map mode-specific-map
             ("l" . org-store-link)
             ;; ("L" . org-insert-link-global)
             ("o" . org-open-at-point-global)
             ("a" . org-agenda)
             ("c" . org-capture))

  ;; Easy navigation
  (setq org-use-speed-commands t
        org-special-ctrl-a/e   t)

  (with-eval-after-load 'org
    (bind-key "C-o" #'helm-org-in-buffer-headings org-mode-map))

  ;; Agenda
  (setq org-agenda-files '("~/Notes/todo"))
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-custom-commands
        '(("i" "Today's TODO" ((tags-todo "+CATEGORY=\"today\"")))))

  ;; Capture
  (setq org-default-notes-file "~/Notes/todo")
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Notes/todo" "Inbox")
           "* %?\n%i\n%a" :empty-lines 1)
          ("l" "Today I Learned" entry (file "~/Notes/TIL.org")
           "* %?\n%u\n%i" :empty-lines 1)
          ("n" "Quote" entry (file+datetree "~/Notes/quote.org")
           "* %?\nEntered on %U\n")
          ("j" "Journal" entry (file+datetree "~/Notes/journal.org")
           "* %?\nEntered on %U\n%i\n%a")))

  ;; Refile
  (setq org-refile-targets (list (cons nil (cons :maxlevel 2))))

  ;; In case this option has been loaded, otherwise `setq' is sufficient
  (customize-set-variable 'org-babel-load-languages
                          '((emacs-lisp . t)
                            (shell      . t)
                            (ruby       . t)
                            (python     . t)
                            (maxima     . t)))

  (setq org-confirm-babel-evaluate nil)
  (setq org-src-tab-acts-natively t)

  ;; (customize-set-variable 'org-export-backends '(html texinfo))

  ;; For Texinfo export
  ;; (setenv "LANG" "en_US.UTF-8")

  ;; For Travis-CI SVG badge in HTML exporting
  (with-eval-after-load 'ox-html
    (seq-doseq (scheme ["file" "http" "https"])
      (add-to-list 'org-html-inline-image-rules
                   (cons scheme "\\.svg\\?branch=master\\'"))))
  )

(require 'org-protocol)

(use-package org
  :disabled t
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c l"   . org-store-link)
         ("C-c C-o" . org-open-at-point-global))
  :config
  ;; Easy navigation
  (setq org-use-speed-commands t)
  (setq org-special-ctrl-a/e t)
  ;; (bind-key "C-o" #'helm-org-headlines org-mode-map)
  ;; Agenda
  (setq org-log-done 'time)
  (setq org-directory           "~/org"
        org-agenda-files        (list "~/org")
        org-default-notes-file  "~/org/todo.org")
  (setq org-agenda-start-with-log-mode t)
  ;; Capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/todo.org")
           "* TODO %?\n  %a\n\n  %i")))

  ;; Dispaly clocking task on OS X menu bar with BitBar
  (defun chunyang-clocking-task ()
    (if (and (fboundp 'org-clocking-p)
             (org-clocking-p))
        (with-temp-buffer
          (insert org-mode-line-string)
          (buffer-substring-no-properties (point-min) (point-max)))
      "[无所事事]")))

(provide 'chunyang-org)
