;; Install helm from Git
(add-to-list 'load-path "~/wip/async")
(add-to-list 'load-path "~/wip/helm")
(setq helm-command-prefix-key "C-c h")
(require 'helm-config)

;; FIXME but also install helm from elpa
(use-package helm :ensure t :defer t)

;;; Setup of Helm's Sub-packages

(use-package helm-mode                  ; Use helm completing everywhere
  :diminish helm-mode
  :config
  (helm-mode)
  (add-to-list 'helm-completing-read-handlers-alist
               '(where-is . helm-completing-read-symbols)))

;;; key bindings, M-0 to M-9
(progn
  (dolist (nth (number-sequence 1 9))
    (define-key helm-map (kbd (format "M-%d" nth))
      ;; In case `lexical-binding' is off
      `(lambda () (interactive) (helm-execute-candidate-action ,nth))))

  ;; Use M-0 for the tenth candidate
  (define-key helm-map (kbd "M-0")
    (lambda () (interactive) (helm-execute-candidate-action 10))))

(defun helm-execute-candidate-action (nth)
  "Move selection to Nth candidate and execute default action."
  (interactive)
  (when (<= nth (helm-get-candidate-number 'in-current-source))
    (let* ((count (- nth (helm-candidate-number-at-point)))
           (direction (if (> count 0) 'next 'previous)))
      (dotimes (_i (abs count))
        (helm-move-selection-common :where 'line
                                    :direction direction))
      (helm-maybe-exit-minibuffer))))

;; (setq helm-candidate-number-limit 100)

(use-package helm-adaptive
  :disabled t
  :config (helm-adaptive-mode))

(use-package helm-command               ; helm-M-x
  :defer t
  :config (setq helm-M-x-always-save-history t))

(use-package helm-buffers
  :defer t
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "^TAGS$")
  (add-to-list 'helm-boring-buffer-regexp-list "git-gutter:diff")

  (define-key helm-buffer-map [?\M-o] #'helm-buffer-switch-other-window))

(use-package helm-files
  :defer t
  :config
  (add-to-list 'helm-boring-file-regexp-list ".DS_Store")

  (define-key helm-find-files-map [?\M-o] #'helm-ff-run-switch-other-window)
  (define-key helm-generic-files-map [?\M-o] #'helm-ff-run-switch-other-window)

  (use-package helm-ls-git
    :ensure t
    :defer t)

  (use-package helm-ls-svn
    :disabled t
    :load-path "~/wip/chunyang/helm-ls-svn.el"
    :bind ("M-8" . helm-ls-svn-ls))

  (use-package helm-fuzzy-find
    :load-path "~/wip/helm-fuzzy-find/"
    :commands helm-fuzzy-find))

(use-package helm-color                 ; Input colors with Helm
  :ensure helm
  :bind ("C-c i C" . helm-colors))

(use-package helm-unicode               ; Unicode input with Helm
  :ensure t
  :bind ("C-c i 8" . helm-unicode))

(use-package helm-grep
  ;; Must make sure `wgrep-helm' is available first and do NOT load it
  ;; since it is soft loaded in `helm-grep'
  :preface (use-package wgrep-helm :ensure t :defer t)
  :defer t
  :bind ("M-I" . helm-grep-do-git-grep)
  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep))

(use-package helm-regexp
  :defer t
  :init (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map))

(use-package helm-ring
  :defer t
  :config
  (add-to-list 'helm-kill-ring-actions
               '("Yank(s)" .
                 (lambda (_candidate)
                   (helm-kill-ring-action
                    (mapconcat #'identity (helm-marked-candidates) "\n"))))))

(use-package helm-man
  :defer t
  :config
  ;; helm needs a relatively new man version, which is not provided on even
  ;; latest OS X (10.10) and also not available on MacPorts
  (setq helm-man-format-switches "%s"))

(use-package helm-elisp                 ; Helm commands for Emacs Lisp
  :bind ("C-c f l" . helm-locate-library))

;; Set up shorter key bindings
(bind-keys ("M-x"                            . helm-M-x)
           ;; File
           ("C-x C-f"                        . helm-find-files)
           ("C-x f"                          . helm-recentf)
           ("C-x C-d"                        . helm-browse-project)
           ;; Buffer
           ([remap switch-to-buffer]         . helm-buffers-list)       ; C-x b
           ("M-l"                            . helm-mini)               ; M-l
           ;; Kill Ring
           ([remap yank-pop]                 . helm-show-kill-ring)     ; M-y
           ("C-z"                            . helm-resume)
           ;; Register
           ("C-x r j"                        . helm-register) ; jump-to-register
           ;; Help
           ([remap apropos-command]          . helm-apropos)            ; C-h a
           ;; Bookmark
           ([remap bookmark-jump]            . helm-filtered-bookmarks) ; C-x r b
           ;; TAGS
           ;; ([remap xref-find-definitions] . helm-etags-select)
           ;;  Mark Ring
           ;; ("C-c <SPC>"                      . helm-all-mark-rings)
           ;; Occur
           ("M-i"                            . helm-occur)
           ;; Imenu
           ("C-o"                            . helm-semantic-or-imenu))

(bind-keys :map helm-command-map
           ("g"   . helm-chrome-bookmarks)
           ("z"   . helm-complex-command-history)
           ("C-/" . helm-fuzzy-find)
           ("G"   . helm-github-stars))

(use-package helm-ag
  :disabled t
  :ensure t
  :bind (("C-c S" . helm-do-ag)    ; C-u chooses file type, C-- enter own option
         ("C-c s" . helm-do-ag-project-root)
         ("C-c C-s" . helm-do-ag-project-root)))

(use-package helm-descbinds
  ;; :ensure t
  :load-path "~/wip/helm-descbinds/"
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode))

(use-package helm-open-github  :ensure t :defer t)

(use-package helm-github-stars
  :ensure t
  :config
  (add-hook 'helm-github-stars-clone-done-hook #'dired)
  (setq helm-github-stars-refetch-time (/ 6.0 24)
        helm-github-stars-full-frame t
        helm-github-stars-default-sources '(hgs/helm-c-source-stars
                                            hgs/helm-c-source-repos)))

(use-package helm-mu
  :ensure t
  :defer t
  :config (setq helm-mu-gnu-sed-program "gsed"
                helm-mu-skip-duplicates t))

(use-package helm-zhihu-daily    :ensure t :defer t)

(use-package helm-org
  :ensure helm
  :defer t
  :config (setq helm-org-headings-fontify t))

(provide 'chunyang-helm)
