;;; Let's Go!
(unless noninteractive
  (message "Loading %s..." load-file-name))

(setq message-log-max 16384)
(setq load-prefer-newer t)                ; Please don't load outdated byte code
(eval-after-load 'advice
  `(setq ad-redefinition-action 'accept)) ; No more warning


;;; Package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar use-package-verbose t)
(require 'use-package)

;; My personal packages
(push (expand-file-name "personal" user-emacs-directory) load-path)


;;; Initialization
(setq inhibit-default-init t)           ; And disable the site default settings

;;; Customization interface
(use-package cus-edit
  :defer t :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'no-error 'no-message))


;;; OS X support
(use-package ns-win
  :if (and (window-system) (eq system-type 'darwin))
  :defer t
  :config (setq ns-pop-up-frames nil     ; Don't pop up new frames from the workspace
                mac-command-modifier 'meta
                mac-option-modifier 'control))

;; http://emacs.stackexchange.com/questions/10570/executing-commands-through-shell-command-what-is-the-path-used
(setq shell-command-switch "-ic")

(use-package exec-path-from-shell
  :ensure t
  :defer 3
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-copy-env "INFOPATH")
  (exec-path-from-shell-copy-env "MANPATH")
  (exec-path-from-shell-initialize))

(use-package info
  :defer t
  :config
  (add-to-list 'Info-directory-list "/opt/local/share/info"))


;;; User interface

;; Get rid of tool bar, menu bar and scroll bars.  On OS X we preserve the menu
;; bar, since the top menu bar is always visible anyway, and we'd just empty it
;; which is rather pointless.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; No startup screen and short Yes/No questions.
(setq ring-bell-function #'ignore
      inhibit-startup-screen t)
(fset 'yes-or-no-p #'y-or-n-p)
;; Opt out from the startup message in the echo area by simply disabling this
;; ridiculously bizarre thing entirely.
(fset 'display-startup-echo-area-message #'ignore)

(and (window-system) (member "Source Code Pro for Powerline" (font-family-list))
     (set-face-attribute 'default nil :font "Source Code Pro for Powerline 13"))

;;; TODO Chinese font setup
;; (when (member "STFangsong" (font-family-list))
;;   (set-fontset-font t 'han (font-spec :family "STFangsong"))
;;   (setq face-font-rescale-alist '(("STFangsong" . 1.3))))

(use-package zenburn-theme                     :ensure t :defer t)
(use-package solarized-theme                   :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow    :ensure t :defer t)


;;; The mode line
(use-package powerline
  :ensure t
  :defer t
  :config
  (setq powerline-display-mule-info nil
        powerline-display-buffer-size t)
  :init
  (powerline-default-theme))

(use-package nyan-mode
  :disabled t
  :ensure t
  :config (nyan-mode 1))


;;; The minibuffer
(use-package async    :ensure t :defer t)
(use-package helm     :ensure t :defer t)

(use-package helm-config
  :defer 3
  :bind (("M-x"                            . helm-M-x)
         ;; File
         ("C-x C-f"                        . helm-find-files)
         ("C-x f"                          . helm-recentf)
         ;; Buffer
         ([remap switch-to-buffer]         . helm-buffers-list)       ; C-x b
         ([remap downcase-word]            . helm-mini)               ; M-l
         ;; Kill ring
         ([remap yank-pop]                 . helm-show-kill-ring)     ; M-y
         ([remap suspend-frame]            . helm-resume)             ; C-z
         ;; Register
         ([remap jump-to-register]         . helm-register)
         ;; Help
         ([remap apropos-command]          . helm-apropos)            ; C-h a
         ;; Bookmark
         ([remap bookmark-jump]            . helm-filtered-bookmarks) ; C-x r b
         ;; Project (Git)
         ([remap list-directory]           . helm-browse-project)     ; C-x C-d
         ;; TAGS
         ;; ([remap xref-find-definitions] . helm-etags-select)
         ("C-c <SPC>"                      . helm-all-mark-rings)
         ("M-i"                            . helm-occur)
         ("C-c h i"                        . helm-semantic-or-imenu))

  :init
  (defvar helm-command-prefix-key "C-c h")

  :config
  (use-package helm-mode
    :defer t
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-adaptive
    :disabled t
    :init (helm-adaptive-mode))

  (bind-key "C-c C-l"    #'helm-minibuffer-history    minibuffer-local-map)
  (bind-key "M-i"        #'helm-occur-from-isearch    isearch-mode-map)
  (bind-keys :map helm-command-map
             ("g" . helm-chrome-bookmarks)
             ("z" . helm-complex-command-history))
  (bind-key "M-I" #'helm-do-grep)

  (defun toggle-small-helm-window ()
    (interactive)
    (if (get 'toggle-small-helm-window 'once)
        (setq display-buffer-alist
              (seq-remove
               (lambda (elt)
                 (and (stringp (car elt))
                      (string-match "helm" (car elt))))
               display-buffer-alist))
      (add-to-list 'display-buffer-alist
                   `(,(rx bos "*helm" (* not-newline) "*" eos)
                     (display-buffer-in-side-window)
                     (inhibit-same-window . t)
                     (window-height . 0.4))))
    (put 'toggle-small-helm-window
         'once (not (get 'toggle-small-helm-window 'once)))))

(use-package helm-command
  :defer t
  :config (setq helm-M-x-always-save-history t))

(use-package helm-regexp
  :defer t
  :config
  (dolist (source '(helm-source-occur helm-source-moccur))
    (push source helm-sources-using-default-as-input)))

(use-package helm-buffers
  :defer t
  :config
  (defmethod helm-setup-user-source :after ((source helm-source-buffers))
    (helm-source-add-action-to-source-if
     "Imenu buffer" (lambda (candidate)
                      (switch-to-buffer candidate)
                      (helm-imenu))
     source (lambda (_candidate) t)))
  (add-to-list 'helm-boring-buffer-regexp-list "TAGS")
  (add-to-list 'helm-boring-buffer-regexp-list "git-gutter:diff"))

(use-package helm-files
  :defer t
  :config
  (add-to-list 'helm-boring-file-regexp-list ".DS_Store")
  (defmethod helm-setup-user-source :after ((source helm-source-ffiles))
    (helm-source-add-action-to-source-if
     "Imenu file" (lambda (candidate)
                    (find-file candidate)
                    (helm-imenu))
     source (lambda (_candidate) t))))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode))

(use-package springboard
  :ensure t
  :bind ("C-." . springboard))

;; Save Minibuffer histroy
(use-package savehist
  :defer 3
  :config
  (setq history-length 1000
        history-delete-duplicates t)
  (savehist-mode 1))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(setq scroll-preserve-screen-position 'always) ; Ensure that M-v always undoes C-v, so you can go back exactly

(use-package popwin
  :ensure t
  :defer t
  :commands popwin-mode
  :init (popwin-mode 1))

(use-package frame
  :bind (("C-c t F" . toggle-frame-fullscreen)
         ("C-c t m" . toggle-frame-maximized))
  :config
  (add-to-list 'initial-frame-alist '(maximized . fullscreen))
  (unbind-key "C-x C-z"))

;;; Note: already enabled by default from Emacs 24.4 (?)
(use-package uniquify                   ; Make buffer names unique
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package windmove
  :defer 7
  :config
  (windmove-default-keybindings))

(use-package desktop                    ; Save buffers, windows and frames
  :defer t :init
  (desktop-save-mode 1))

(use-package winner
  :defer 7
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t R" . writeroom-mode)))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package dired                      ; Edit directories
  :defer t
  :config
  ;; VCS integration with `diff-hl'
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  (setq insert-directory-program "/opt/local/bin/gls")

  (use-package dired-x
    :commands dired-omit-mode
    :defer t :init
    (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))))

(use-package direx
  :disabled t
  :ensure t
  :config
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config)
  (bind-key "C-x C-J" #'direx:jump-to-directory-other-window))

(use-package bookmark
  :defer t
  :config (setq bookmark-save-flag 1))

(use-package recentf                    ; Save recently visited files
  :defer t
  :config
  (setq recentf-max-saved-items 50
        ;; Cleanup recent files only when Emacs is idle, but not when the mode
        ;; is enabled, because that unnecessarily slows down Emacs. My Emacs
        ;; idles often enough to have the recent files list clean up regularly
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              "/itsalltext/"  ; It's all text temp files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'"))
  (recentf-mode))

(use-package saveplace                  ; Save point position in files
  :defer t :init
  (setq-default save-place t))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :defer t :init
  (global-auto-revert-mode))


;;; Basic editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(use-package electric                   ; Electric code layout
  :defer t :init
  (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :defer t :init
  (electric-pair-mode))

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)
;; (add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)

(use-package chunyang-simple
  :commands (demo
             chunyang-git-clone
             chunyang-run-command-in-iterm
             chunyang-reply-smth
             chunyang-save-scratch
             chunyang-restore-scratch)
  :bind (([remap split-window-right] . chunyang-split-window-right)
         ([remap split-window-below] . chunyang-split-window-below)
         ("M-o"                      . chunyang-other-window))
  :init
  (add-hook 'kill-emacs-hook #'chunyang-save-scratch))

(use-package easy-repeat
  :ensure t :defer t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :disabled t
  :ensure t
  :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :disabled t
  :ensure t
  :config
  (setq visual-fill-column-disable-fringe nil)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package zop-to-char
  :ensure t
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-z"              . zop-up-to-char)))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp]      . easy-mark)))

(use-package expand-region              ; Expand region by semantic units
  :disabled t
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package undo-tree                  ; Branching undo
  :disabled t
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

;; Give us narrowing back!
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'view-hello-file
     'disabled "I mistype C-h h a lot and it is too slow to block Emacs")


;;; Navigation and scrolling
(setq scroll-margin 0                   ; Drag the point along while scrolling
      scroll-conservatively 1000        ; Never recenter the screen while scrolling
      scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; These settings make trackpad scrolling on OS X much more predictable
      ;; and smooth
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :diminish page-break-lines-mode
  :defer t :init
  (global-page-break-lines-mode))

(use-package outline                    ; Navigate outlines in buffers
  :disabled t
  :diminish outline-minor-mode
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'outline-minor-mode)))

(use-package imenu-anywhere             ; Helm-based imenu across open buffers
  :disabled t
  :ensure t
  :bind (("C-c i" . helm-imenu-anywhere)))

(use-package imenu-list
  :ensure t
  :commands (imenu-list imenu-list-minor-mode))

(use-package origami			; Code folding
  :ensure t
  :commands (origami-mode global-origami-mode))


;;; Search
(setq isearch-allow-scroll t)

(use-package pinyin-search :ensure t :defer t)

(use-package grep
  :defer t
  :config
  (dolist (file '("TAGS" "GPATH" "GRTAGS" "GTAGS"))
    (add-to-list 'grep-find-ignored-files file)))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :diminish anzu-mode
  :defer t :init
  (global-anzu-mode +1)
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp))


;;; Highlights
(use-package hl-line
  :bind ("C-c t L" . hl-line-mode)
  :init
  (use-package hl-line+ :ensure t :defer t))

(use-package paren                      ; Highlight paired delimiters
  :defer t :init
  (show-paren-mode 1))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hl-todo
  :ensure t
  :defer t :init
  (global-hl-todo-mode))

(use-package color-identifiers-mode
  :ensure t
  :diminish color-identifiers-mode
  :bind ("C-c t c" . global-color-identifiers-mode))


;;; Skeletons, completion and expansion
(use-package hippie-exp                 ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(use-package company                    ; Graphical (auto-)completion
  :ensure t
  :diminish company-mode
  :defer t
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  :init (global-company-mode))

(use-package yasnippet
  :ensure t
  :defer t)


;;; Spelling and syntax checking
(use-package flyspell
  :bind (("C-c i b" . flyspell-buffer)
         ("C-c i f" . flyspell-mode)
         ("C-c t i" . chunyang-flyspell))
  :init
  (use-package ispell
    :defer t
    :config
    (setq ispell-program-name "aspell"  ; use aspell instead of ispell
          ispell-extra-args '("--sug-mode=ultra")))

  (defun chunyang-flyspell (arg)
    "Enable flyspell as much as possible."
    (interactive "P")
    (if arg
        (progn
          (remove-hook 'text-mode-hook #'flyspell-mode)
          (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
          (message "Disable flyspell"))
      (add-hook 'text-mode-hook #'flyspell-mode)
      (add-hook 'prog-mode-hook #'flyspell-prog-mode)
      (message "Enable flyspell")))

  :config
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map))

(use-package flycheck
  :ensure t
  :bind (("C-c t f" . global-flycheck-mode)
         ("C-c L e" . list-flycheck-errors))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Configuring buffer display in Emacs
  ;; http://www.lunaryorn.com/2015/04/29/the-power-of-display-buffer-alist.html
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4)))

  (defun lunaryorn-quit-bottom-side-windows ()
    "Quit side windows of the current frame."
    (interactive)
    (dolist (window (window-at-side-list))
      (quit-window nil window)))

  (global-set-key (kbd "C-c q") #'lunaryorn-quit-bottom-side-windows)

  (use-package flycheck-pos-tip           ; Show Flycheck messages in popups
    :ensure t
    :config (setq flycheck-display-errors-function
                  #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :ensure t
    :config
    (eval-after-load "flycheck"
      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))


;;; Text editing
(use-package iedit
  :disabled t                           ; TODO: read manual
  :ensure t
  :config
  (bind-key [C-return] #'iedit-rectangle-mode))


;;; Other markup languages
(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.mdpp\\'"        . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :config (setq markdown-command "kramdown"))

(use-package yaml-mode
  :ensure t
  :defer t)


;;; Programming utilities
(use-package compile
  :bind (("C-c C" . compile)
         ("M-O"   . show-compilation))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*compilation\\*" (buffer-name buf))
                   (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config
  (setq compilation-ask-about-save nil         ; Just save before compiling
        compilation-always-kill t
        compilation-scroll-output 'first-error ; Automatically scroll to first error
        )
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))

(use-package highlight-numbers          ; Fontify number literals
  :disabled t
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :diminish highlight-symbol-mode
  :defer t :init
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  ;; Highlight symbol occurrences
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :bind (("C-c t r" . rainbow-mode))
  :config (add-hook 'css-mode-hook #'rainbow-mode))

(use-package quickrun
  :ensure t :defer t
  :config (push "*quickrun*" popwin:special-display-config))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (unbind-key "M-r" paredit-mode-map) (bind-key "M-R" #'paredit-raise-sexp  paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map) (bind-key "M-S" #'paredit-splice-sexp paredit-mode-map)
  (unbind-key "C-j" paredit-mode-map)

  (use-package paredit-menu
    :ensure t
    :commands menubar-paredit))


;;; Emacs Lisp
(use-package lisp-mode
  :defer t
  :preface
  (defadvice pp-display-expression (after make-read-only (expression out-buffer-name) activate)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1))))

  (defun string-first-line (string)
    (and (stringp string)
         (string-match ".*$" string)
         (match-string 0 string)))

  (defun chunyang-elisp-function-or-variable-quickhelp (symbol)
    "Display a short documentation of the function or variable using `popup'.

See also `describe-function-or-variable'."
    (interactive
     (let* ((v-or-f (variable-at-point))
            (found (symbolp v-or-f))
            (v-or-f (if found v-or-f (function-called-at-point)))
            (found (or found v-or-f)))
       (list v-or-f)))
    (if (not (and symbol (symbolp symbol)))
        (message "You didn't specify a function or variable.")
      (let* ((fdoc (when (fboundp symbol)
                     (or (documentation symbol t) "Not documented.")))
             (fdoc-short (string-first-line fdoc))
             (vdoc (when  (boundp symbol)
                     (or (documentation-property symbol 'variable-documentation t)
                         "Not documented as a variable.")))
             (vdoc-short (string-first-line vdoc)))
        (and (require 'popup nil 'no-error)
             (popup-tip
              (or
               (and fdoc-short vdoc-short
                    (concat fdoc-short "\n\n"
                            (make-string 30 ?-) "\n" (symbol-name symbol)
                            " is also a " "variable." "\n\n"
                            vdoc-short))
               fdoc-short
               vdoc-short)
              :margin t)))))

  (defun imenu-use-package ()
    (add-to-list
     'imenu-generic-expression
     '("Package" "^\\s-*(use-package\\s-+\\(\\(\\sw\\|\\s_\\)+\\)[[:space:]
]+[^)]" 1) t))

  :config
  (bind-key "C-h C-." #'chunyang-elisp-function-or-variable-quickhelp)
  (bind-key "M-:"     #'pp-eval-expression)
  (bind-key "C-c t d" #'toggle-debug-on-error)

  (use-package rebox2
    :ensure t
    :diminish rebox-mode
    :bind ([(meta q)] . rebox-dwim)
    :preface
    (defun chunyang--elisp-comment-setup ()
      (setq-local rebox-style-loop '(21 23 25 27))
      (setq-local rebox-min-fill-column 40)))

  ;; TODO make my own hook func
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'ipretty-mode)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode))
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'chunyang--elisp-comment-setup)
  (add-hook 'emacs-lisp-mode-hook #'imenu-use-package))

(use-package eshell
  :bind  ("C-!" . eshell-command))

(use-package aggressive-indent
  :ensure t
  :defer t
  :diminish aggressive-indent-mode)

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :bind ("C-h ." . elisp-slime-nav-describe-elisp-thing-at-point))

(use-package ipretty             :ensure t :defer t)
(use-package pcache              :ensure t :defer t)
(use-package persistent-soft     :ensure t :defer t)
(use-package command-log-mode    :ensure t :defer t)
(use-package log4e               :ensure t :defer t)
(use-package alert               :ensure t :defer t)
(use-package bug-hunter          :ensure t :defer t)


;;; Common Lisp
(use-package slime
  :disabled t
  :ensure t)


;;; Haskell
(use-package haskell-mode
  :disabled t
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))


;;; Scheme
(use-package geiser
  :disabled t
  :ensure t
  :config
  ;; geiser replies on a REPL to provide autodoc and completion
  (setq geiser-mode-start-repl-p t)
  :init
  (add-hook 'scheme-mode-hook (lambda () (paredit-mode))))


;;; Ruby


;;; C
(use-package ggtags
  :ensure t :defer t
  :config
  (defun chunyang--c-setup ()
    (when (derived-mode-p 'c-mode 'c++-mode)
      (ggtags-mode 1))
    (setq-local imenu-create-index-function #'ggtags-build-imenu-index))
  (add-hook 'c-mode-common-hook #'chunyang--c-setup))


;;; Version control
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :bind ("C-x C-g" . git-gutter:toggle)
  :config
  (bind-keys ("C-x v p" . git-gutter:previous-hunk)
             ("C-x v n" . git-gutter:next-hunk)
             ("C-x v s" . git-gutter:stage-hunk)
             ("C-x v r" . git-gutter:revert-hunk))
  (setq git-gutter:handled-backends '(git svn))
  (global-git-gutter-mode t))

(use-package git-messenger
  :ensure t
  :bind ("C-x v P" . git-messenger:popup-messagew))

(use-package magit                      ; The one and only Git frontend
  :ensure t
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config (setq magit-save-some-buffers 'dontask))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind ("C-x v t" . git-timemachine))


;;; Tools and utilities
(use-package server
  :defer 7
  :config
  (unless (server-running-p) (server-start)))

(use-package projectile                 ; Project management
  :ensure t
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq projectile-mode-line '(:eval (if (condition-case nil
                                             (and projectile-require-project-root
                                                  (projectile-project-root))
                                           (error nil))
                                         (format " Project[%s]"
                                                 (projectile-project-name))
                                       "")))
  (use-package helm-projectile
    :ensure t :defer t :init
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    (use-package helm-ag :ensure t :defer t))

  (projectile-global-mode))

(use-package helm-open-github
  :disabled t
  :ensure t
  :commands (helm-open-github-from-file
             helm-open-github-from-issues
             helm-open-github-from-commit
             helm-open-github-from-pull-requests))

(use-package helm-github-stars
  :ensure t
  :defer t
  :config
  (add-hook 'helm-github-stars-clone-done-hook #'dired)
  (setq helm-github-stars-cache-file "~/.emacs.d/var/hgs-cache"
        helm-github-stars-refetch-time (/ 6.0 24))
  (bind-key "G" #'helm-github-stars helm-command-map))

(use-package jist                       ; Gist
  :ensure t
  :commands jist-list
  :config (load-file "~/.private.el"))

(use-package paradox                    ; Better package menu
  :ensure t
  :bind (("C-c L p" . paradox-list-packages)
         ("C-c L P" . package-list-packages-no-fetch))
  :config
  (setq paradox-github-token t
        paradox-execute-asynchronously nil))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :commands guide-key-mode
  :defer 7
  :config
  (setq guide-key/guide-key-sequence
        '("C-h"                         ; Help
          "C-x r"                       ; Registers and Rectangle
          "C-x 4"                       ; other-window
          "C-c h"                       ; Helm
          "C-x n"                       ; Narrowing
          "C-c p"                       ; Projectile
          "C-c t"                       ; Personal Toggle commands
          "C-c L"                       ; Personal List something commands
          "C-c f"                       ; File
          "C-x v"                       ; VCS
          "C-c A"                       ; Align
          ))
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (guide-key-mode 1))

(use-package keyfreq
  :disabled t
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package hydra            :ensure t :defer t :disabled t)
(use-package dash-at-point    :ensure t :defer t)
(use-package helm-dash        :ensure t :defer t)


;;; Net & Web & Email
(use-package rcirc
  :defer t
  :config
  (setq rcirc-default-nick "chunyang")
  (setq rcirc-log-flag t)
  (add-to-list 'rcirc-server-alist
               '("irc.freenode.net"
                 :channels ("#macports-gsoc")))
  (load-file  "~/.private.el")
  (add-hook 'rcirc-mode-hook #'flyspell-mode)
  (rcirc-track-minor-mode))

(use-package mu4e
  :load-path "/opt/local/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  ;; Creating org-mode links
  (use-package org-mu4e)

  (setq mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash")
  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  ;; skip duplicate messages (caused by the combination of Gmail and offlineimap)
  (setq mu4e-headers-skip-duplicates t)
  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/INBOX"               . ?i)
          ("/[Gmail].Sent Mail"   . ?s)))
  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "proxychains4 offlineimap"
        mu4e-update-interval (* 15 60)  ; update every 15 minutes
        )

  ;; show images
  (setq mu4e-show-images t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; convert html emails properly
  ;; Possible options:
  ;;   - html2text -utf8 -width 72
  ;;   - textutil -stdin -format html -convert txt -stdout
  ;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
  ;;   - w3m -dump -cols 80 -T text/html
  ;;   - view in browser (provided below)
  (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))

  ;; something about ourselves
  (setq user-mail-address "xuchunyang56@gmail.com"
        user-full-name  "Chunyang Xu"
        mu4e-compose-signature
        (concat
         "Cheers,\n"
         "Chunyang Xu\n"))
  ;; Send via msmtp (for socks proxy support)
  (setq message-sendmail-f-is-evil 't)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments (list '"-a" "default"))
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t))

(use-package sx                  :ensure t :defer t)
(use-package helm-zhihu-daily    :ensure t :defer t)

(use-package weibo
  :disabled t
  :ensure t
  :config (load-file "~/.private.el"))

(use-package google-this
  :disabled t
  :defer t
  :ensure t
  :diminish google-this-mode
  :bind-keymap ("C-c g" . google-this-mode-submap)
  :config
  (google-this-mode))


;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point+))
  :config
  (setq url-automatic-caching t)
  (push "*Youdao Dictionary*" popwin:special-display-config))

(use-package trans
  :bind (("C-c g"   . trans)
         ("C-c G"   . trans-popup)
         ("C-c C-g" . trans-message))
  :config
  (setq trans-command "proxychains4 -q ~/repos/translate-shell/translate"))

(use-package osx-dictionary
  :ensure t
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))


;;; MacPorts related tools
(use-package tcl-mode
  :mode "Portfile")

(bind-key "C-h C-k" #'find-function-on-key)


;;; Web Development
(use-package restclient :ensure t :defer t)


;;; org-mode
(use-package org
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c l"   . org-store-link)
         ("C-c b"   . org-iswitchb)
         ("C-c C-o" . org-open-at-point-global))

  :config
  (setq org-directory "~/Dropbox/Notes")
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (setq org-agenda-files `(,org-default-notes-file))

  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline org-default-notes-file "Notes")
           "* %?\n  %i\n  %a")))

  (setq org-agenda-custom-commands
        '(("E" "Agenda and Emacs-related tasks"
           ((agenda "")
            (tags-todo "emacs")))
          ("g" "Agenda and GSoC-related tasks"
           ((agenda "")
            (tags "gsoc")))))

  ;; Clock work time
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)

  (use-package org-mac-link
    :if (eq system-type 'darwin)
    :ensure t
    :commands (org-mac-chrome-insert-frontmost-url))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)))
  (setq org-confirm-babel-evaluate nil)

  (setq org-edit-src-auto-save-idle-delay 5)

  (setq org-src-fontify-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-tab-acts-natively t)

  (defun chunyang-org-make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "remember") (width . 80) (height . 16)
                  (top . 400) (left . 300)))
    (select-frame-by-name "remember")
    (org-capture))

  ;;,------------------------------------------------------------------------------------
  ;;| Show org-mode clock in Mac OS X menubar
  ;;| [[https://github.com/koddo/org-clock-statusbar-app][koddo/org-clock-statusbar-app]]
  ;;`------------------------------------------------------------------------------------
  (add-hook 'org-clock-out-hook
            (lambda ()
              (call-process
               "/usr/bin/osascript" nil 0 nil
               "-e" "tell application \"org-clock-statusbar\" to clock out")))
  (add-hook 'org-clock-in-hook
            (lambda ()
              (call-process
               "/usr/bin/osascript" nil 0 nil
               "-e"
               (concat
                "tell application \"org-clock-statusbar\" to clock in \""
                org-clock-current-task
                "\"")))))

(use-package orglink
  :ensure t
  :diminish orglink-mode
  :defer t :init (global-orglink-mode))

(use-package org-bullets
  :disabled t
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package calfw
  :ensure t :defer t
  :init (use-package calfw-org :defer 5))

(bind-key "C-h h" #'describe-personal-keybindings)
