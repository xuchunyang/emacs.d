;; Install helm from Git
(add-to-list 'load-path "~/Projects/emacs-async")
(add-to-list 'load-path "~/Projects/helm")
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

;; Defaults to 15, not working correctly in helm-occur, so disable it :(
(setq helm-highlight-matches-around-point-max-lines 0)

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
    :ensure t
    :bind ("M-8" . helm-ls-svn-ls))

  (use-package helm-fuzzy-find
    :disabled t
    :ensure t
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
  :preface
  (use-package wgrep-helm :ensure t :defer t)
  (defun chunyang/helm-do-grep-ag (arg)
    (interactive "P")
    (let ((dir (or (ignore-errors (projectile-project-root))
                   default-directory)))
      (helm-grep-ag dir arg)))
  :defer t
  :bind ("M-I" . chunyang/helm-do-grep-ag)
  :config
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)
  (setq helm-grep-ag-command
        "ag --smart-case --line-numbers --nogroup --color-match='0;32' %s %s %s"))

(use-package helm-regexp
  :defer t
  :init (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)
  :config
  (defun isearch-from-helm-occur ()
    (interactive)
    (helm-run-after-exit
     (lambda (initial)
       (isearch-forward nil t)
       (isearch-yank-string initial))
     helm-pattern))
  (define-key helm-moccur-map "\C-s" #'isearch-from-helm-occur))

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
(bind-keys ("M-x"     . helm-M-x)
           ("C-x C-f" . helm-find-files)
           ("C-x f"   . helm-recentf)
           ("C-x C-d" . helm-browse-project)
           ("M-l"     . helm-mini)
           ("M-y"     . helm-show-kill-ring)
           ("C-z"     . helm-resume)
           ("C-x r j" . helm-register)
           ("C-h a"   . helm-apropos)
           ("M-i"     . helm-occur)
           ("C-o"     . helm-semantic-or-imenu))

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
  :ensure t
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


;;; Manage advises
(defun advice--members (symbol)
  (let ((definition (advice--symbol-function symbol))
        (fns '()))
    (while (advice--p definition)
      (push (advice--car definition) fns)
      (setq definition (advice--cdr definition)))
    (nreverse fns)))

(defun helm-manage-nadvice ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Advices"
          :candidates
          (cl-remove-if-not
           (lambda (func-name)
             (and (featurep 'nadvice)
                  (advice--p (advice--symbol-function (intern func-name)))))
           (all-completions "" obarray #'fboundp))
          :coerce #'intern
          :action
          (lambda (candidate)
            (run-at-time
             0.01 nil
             (lambda (symbol)
               (message "`%s' is advised by `%s'"
                        symbol
                        (advice--members symbol))
               (helm :sources
                     (helm-build-sync-source (format "Remove advice(s) from %s" symbol)
                       :candidates (mapcar #'symbol-name (advice--members symbol))
                       :coerce #'intern
                       :action (lambda (ad) (advice-remove symbol ad)))))
             candidate)))))


;;; Manage Emacs's hook
(defun helm-manage-hooks ()
  ;; Note: It's much better to add a custom action to `helm-apropos', however,
  ;; its action is not customizable and I'm not sure this function is useful for
  ;; other helm users.
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (helm-build-sync-source "Choose hook"
            :candidates
            (all-completions "" obarray
                             (lambda (x)
                               (and (boundp x) (not (keywordp x))
                                    (string-match "-hook$" (symbol-name x)))))
            ;; TODO: This is needed to make sure :preselect is working (which
            ;; might be a bug of helm)
            :candidate-number-limit 9999
            :action
            (lambda (candidate)
              (run-at-time 0.01 nil
                           (lambda (hook)
                             (helm :sources
                                   (helm-build-sync-source (format "Manage function(s) from %s" hook)
                                     :candidates (mapcar #'symbol-name (symbol-value hook))
                                     :action (helm-make-actions
                                              "Remove this function from hook"
                                              (lambda (candidate)
                                                ;; Warn: I'm not handling the LOCAL argument
                                                (remove-hook hook (intern-soft candidate)))))))
                           (intern-soft candidate))))
          :preselect default)))


;; Utilities
(defmacro helm-string (string)
  `(helm :sources (helm-build-in-buffer-source "Helm String"
                    :data ,string)))

(defmacro helm-list (list)
  (let ((tempvar (make-symbol "stringify-list")))
    `(let ((,tempvar (mapcar (lambda (obj) (format "%s" obj)) ,list)))
       (helm :sources (helm-build-sync-source "Helm List"
                        :candidates ,tempvar)))))

(defun helm-shell-command (command)
  (interactive (list (read-shell-command "Shell command: ")))
  (helm-string (shell-command-to-string command)))

(provide 'chunyang-helm)
