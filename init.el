;;; init.el --- Emacs configuration of Chunyang Xu  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015 Chunyang Xu <xuchunyang56@gmail.com>
;;
;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/emacs.d
;;
;;; License: GPLv3

;;; Commentary:

;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c h: Helm
;; - C-c L: List things
;; - C-c t: Toggle things
;; - C-x v: VCS
;; - C-c /: Google Search

;;; Code:

(unless noninteractive
  (message "Loading %s..." load-file-name))
(setq message-log-max 16384)

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))


;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-archive-priorities '(("gnu" . 20) ("melpa" . 0)))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;; My personal packages
(push (expand-file-name "personal" user-emacs-directory) load-path)


;;; Initialization
(setq inhibit-default-init t)           ; And disable the site default settings


;;; Customization interface
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)


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
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :config
  (exec-path-from-shell-copy-env "INFOPATH")
  (exec-path-from-shell-copy-env "MANPATH")
  (exec-path-from-shell-initialize))

(use-package info
  :defer t
  :config
  (add-to-list 'Info-directory-list "/opt/local/share/info"))

(use-package osx-trash                  ; Trash support for OS X
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))


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
  :disabled t
  :ensure t
  :config
  (setq powerline-display-mule-info nil
        powerline-display-buffer-size t)
  :init
  (powerline-default-theme))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
  (column-number-mode))


;;; The minibuffer
(use-package helm
  :ensure t
  :config
  (setq helm-split-window-default-side 'other))

(use-package helm-config
  :init
  (defvar helm-command-prefix-key "C-c h")
  :config
  (bind-keys :map helm-command-map
             ("g"   . helm-chrome-bookmarks)
             ("z"   . helm-complex-command-history)
             ("C-/" . helm-fuzzy-find)
             ("G" #'helm-github-stars helm-command-map))
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

(use-package helm-mode
  :diminish helm-mode
  :init (helm-mode))

(require 'helm-misc)
(require 'helm-command)
(require 'helm-imenu)
(require 'helm-semantic)
(require 'helm-ring)

(use-package helm-adaptive
  :config (helm-adaptive-mode))

(use-package helm-regexp
  :defer t
  :config
  (dolist (source '(helm-source-occur helm-source-moccur))
    (push source helm-sources-using-default-as-input)))

(use-package helm-command
  :defer t
  :config (setq helm-M-x-always-save-history t))

(use-package wgrep-helm :ensure t :defer t)

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
  :bind ("C-c p h" . helm-browse-project)
  :config
  (add-to-list 'helm-boring-file-regexp-list ".DS_Store")
  (defmethod helm-setup-user-source :after ((source helm-source-ffiles))
    (helm-source-add-action-to-source-if
     "Imenu file" (lambda (candidate)
                    (find-file candidate)
                    (helm-imenu))
     source (lambda (_candidate) t)))

  (use-package helm-ls-git :ensure t :defer t)

  (use-package helm-ls-svn
    :load-path "~/wip/chunyang/helm-ls-svn.el"
    :bind ("M-8" . helm-ls-svn-ls))

  (use-package helm-fuzzy-find
    :load-path "~/wip/helm-fuzzy-find/"
    :commands helm-fuzzy-find))

(bind-keys ("M-x"                            . helm-M-x)
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
           ("C-c i"                          . helm-semantic-or-imenu))

;; `helm-regexp.el'
(bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)
;; `helm-misc.el'
(bind-key "C-c C-l"    #'helm-minibuffer-history    minibuffer-local-map)

(use-package helm-man
  :defer t
  :config (setq helm-man-format-switches "%s"))

(use-package helm-ag
  :ensure t
  :bind ("C-c p s" . helm-do-ag-project-root))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds)
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode))

(use-package springboard
  :disabled t                           ; Don't use it for a long time and
                                        ; occupy a handy shortcuts.
  :ensure t
  :bind ("C-." . springboard))

;; Save Minibuffer histroy
(use-package savehist
  :init
  (setq history-length 1000
        history-delete-duplicates t)
  (savehist-mode))


;;; Buffer, Windows and Frames

(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

;; Ensure that M-v always undoes C-v, so you can go back exactly
;; (setq scroll-preserve-screen-position 'always)

(use-package popwin
  :ensure t
  ;; :commands popwin-mode
  ;; :init (popwin-mode)
  )

(use-package frame
  :bind (("C-c t F" . toggle-frame-fullscreen)
         ("C-c t m" . toggle-frame-maximized))
  :config
  (add-to-list 'initial-frame-alist '(maximized . fullscreen))
  (unbind-key "C-x C-z"))

;;; Note: already enabled by default from Emacs 24.4 (?)
(use-package uniquify                   ; Make buffer names unique
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package windmove
  :defer 7
  :config
  (windmove-default-keybindings))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config
  (add-to-list 'desktop-globals-to-save 'translate-shell-cache)
  (add-to-list 'desktop-globals-to-save 'translate-shell-brief-cache))

(use-package winner
  :defer 7
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode))

(use-package writeroom-mode             ; Distraction-free editing
  :ensure t
  :bind (("C-c t R" . writeroom-mode)))


;;; File handling

;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files
  :bind (("C-c f u" . revert-buffer)
         ("C-c f n" . normal-mode))
  :config
  ;; FIXME: shoud not hard code
  (setq insert-directory-program "/opt/local/bin/gls"))

;;; Additional bindings for built-ins
(bind-key "C-c f v l" #'add-file-local-variable)
(bind-key "C-c f v p" #'add-file-local-variable-prop-line)

(use-package dired                      ; Edit directories
  :defer t
  :config
  (use-package dired-x
    :commands dired-omit-mode
    :init (add-hook 'dired-mode-hook (lambda () (dired-omit-mode))))
  (use-package dired-subtree :ensure t :defer t)
  ;; VCS integration with `diff-hl'
  (use-package diff-hl
    :ensure t
    :defer t
    :init (add-hook 'dired-mode-hook #'diff-hl-dired-mode)))

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
  (setq recentf-max-saved-items 200
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
  :init (save-place-mode))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))


;;; Basic editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(use-package electric                   ; Electric code layout
  :init (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :init (electric-pair-mode))

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
             chunyang-save-scratch
             chunyang-restore-scratch)
  :bind (([remap split-window-right] . chunyang-split-window-right)
         ([remap split-window-below] . chunyang-split-window-below)
         ("M-o"                      . chunyang-other-window)
         ("C-c f w"                  . chunyang-copy-buffer-name-as-kill)
         ("C-M-!"                    . iterm-shell-command))
  :init (add-hook 'kill-emacs-hook #'chunyang-save-scratch)
  :config
  (require 'easymenu)
  (easy-menu-add-item
   nil '("tools")
   ["iTerm Shell Command..." iterm-shell-command t]
   "Shell Command...")

  (easy-menu-add-item
   nil '("Tools")
   '("Your tools"
     ["Git clone" chunyang-git-clone t]
     ["Download file" chunyang-git-clone t]
     ["Copy buffer name" chunyang-copy-buffer-name-as-kill t]
     "----"
     ["Youdao Dictionary" youdao-dictionary-search-at-point t]
     ["Mac Dictionary" osx-dictionary-search-pointer t]
     ["Google Translate" translate-shell t]
     ["Bing Translate" bing-dict-brief t]))
  (easy-menu-add-item nil '("Tools") '("----") "Your tools"))

(use-package easy-repeat :ensure t :defer t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :defer t
  :init (add-hook 'prog-mode-hook 'ws-butler-mode))

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package zop-to-char
  :ensure t
  :bind (([remap zap-to-char] . zop-to-char)
         ("M-z"               . zop-up-to-char)))

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
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config))

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
  :defer t
  :init (add-hook 'prog-mode-hook #'page-break-lines-mode))

(use-package outline                    ; Navigate outlines in buffers
  :disabled t
  :diminish outline-minor-mode
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'outline-minor-mode)))

(use-package imenu
  :init
  (defun imenu-use-package ()
    (add-to-list 'imenu-generic-expression
                 '("Packages"
                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'imenu-use-package))

(use-package imenu-anywhere             ; Helm-based imenu across open buffers
  :ensure t
  :bind ("C-c I" . helm-imenu-anywhere))

(use-package imenu-list :ensure t :defer t)

(use-package origami :ensure t :defer t)


;;; Search
(setq isearch-allow-scroll t)

(use-package pinyin-search :ensure t :defer t)

(use-package grep
  :defer t
  :config
  (dolist (file '("TAGS" "GPATH" "GRTAGS" "GTAGS"))
    (add-to-list 'grep-find-ignored-files file))
  (use-package wgrep :ensure t :defer t) ; conf done by package.el (autload)
  )

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :diminish anzu-mode
  :init
  (global-anzu-mode +1)
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp))

(use-package which-func                 ; Current function name in header line
  :init (which-function-mode)
  :config
  (setq which-func-unknown "⊥" ; The default is really boring…
        which-func-format
        `((:propertize (" ➤ " which-func-current)
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight
                       help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))))


;;; Highlights
(use-package whitespace                 ; Highlight bad whitespace (tab)
  :bind ("C-c t w" . whitespace-mode))

(use-package hl-line
  :bind ("C-c t L" . hl-line-mode)
  :init
  (use-package hl-line+ :ensure t :defer t))

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
          (add-hook hook #'rainbow-delimiters-mode)))

(use-package hl-todo
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode))

(use-package color-identifiers-mode
  :ensure t
  :diminish color-identifiers-mode
  :bind ("C-c t c" . global-color-identifiers-mode)
  :init (add-hook 'after-init-hook #'global-color-identifiers-mode))


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
  :commands company-complete
  :init (global-company-mode)
  :config
  ;; Use Company for completion
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t)
  (dolist (hook '(git-commit-mode-hook mail-mode-hook))
    (add-hook hook (lambda ()
                     (setq-local company-backends '(company-ispell))))))

(use-package auto-complete
  :disabled t
  :ensure t
  :config
  (ac-config-default)
  (setq ac-auto-show-menu 0.3
        ;; ac-delay 0.1
        ac-quick-help-delay 0.5)
  (use-package ac-ispell
    :ensure t
    :config
    ;; Completion words longer than 4 characters
    (setq ac-ispell-requires 4
          ac-ispell-fuzzy-limit 2)

    (eval-after-load "auto-complete"
      '(progn
         (ac-ispell-setup)))

    (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
    (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)))

(use-package yasnippet :ensure t :defer t)


;;; Spelling and syntax checking
(use-package flyspell
  :diminish flyspell-mode
  :init
  (use-package ispell
    :config (setq ispell-program-name "aspell"
                  ispell-extra-args '("--sug-mode=ultra")))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  (use-package helm-flyspell
    :ensure t
    :init
    (bind-key "C-." #'helm-flyspell-correct flyspell-mode-map))
  (use-package flyspell-popup
    :load-path "~/wip/flyspell-popup"
    :config
    (bind-key "C-." #'flyspell-popup-correct flyspell-mode-map)))

(use-package writegood-mode :ensure t :defer t)

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

(use-package helm-flycheck
  :ensure t
  :bind (("C-c ! L" . helm-flycheck)))


;;; Text editing
(use-package iedit
  :disabled t                           ; TODO: read manual
  :ensure t
  :config (bind-key [C-return] #'iedit-rectangle-mode))


;;; Other markup languages
(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.mdpp\\'"        . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :config (setq markdown-command "kramdown"))

(use-package yaml-mode :ensure t :defer t)


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
  :defer t
  :init
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

(use-package prog-mode                  ; Prog Mode
  :bind (("C-c t p" . prettify-symbols-mode)))


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :config
  (unbind-key "M-r" paredit-mode-map) (bind-key "M-R" #'paredit-raise-sexp  paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map) (bind-key "M-S" #'paredit-splice-sexp paredit-mode-map)
  (unbind-key "C-j" paredit-mode-map)
  (unbind-key "M-q" paredit-mode-map)

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
        (view-mode))))

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

  :config
  (bind-key "C-h C-." #'chunyang-elisp-function-or-variable-quickhelp)
  (bind-key "M-:"     #'pp-eval-expression)
  (bind-key "C-c t d" #'toggle-debug-on-error)

  (use-package rebox2
    :ensure t
    :diminish rebox-mode
    :bind ("M-q" . rebox-dwim)
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
  (add-hook 'emacs-lisp-mode-hook #'chunyang--elisp-comment-setup))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

(use-package eshell
  :preface
  (defun eshell* ()
    "Start a new eshell even if one is active."
    (interactive)
    (eshell t))
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (defun eshell/j (&optional initial-input)
    (interactive)
    (let ((dirs
           (delete-dups
            (mapcar #'expand-file-name (ring-elements eshell-last-dir-ring)))))
      (helm :sources
            (helm-build-sync-source "cd to recent eshell directories"
              :candidates dirs
              :action (lambda (candidate) (eshell/cd candidate)))
            :input initial-input)))
  :bind  (("C-!"   . eshell-command)
          ("C-x m" . eshell)
          ("C-x M" . eshell*))
  :config
  (setq eshell-history-size 5000)       ; Same as $HISTSIZE
  (setq eshell-hist-ignoredups t)       ; make the input history more bash-like

  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))
                                        ; (I don't know what this means)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys :map eshell-mode-map
                         ([remap eshell-pcomplete] . helm-esh-pcomplete)
                         ("M-p"                    . helm-eshell-history)
                         ("C-l"                    . eshell-clear-buffer)
                         ("C-c C-k"                . compile)
                         ("C-c C-q"                . eshell-kill-process))
              (eshell/export "EDITOR=emacsclient -n")
              (eshell/export "VISUAL=emacsclient -n"))))

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
  :ensure t
  :init
  (defun chunyang--setup-ggtags ()
    (ggtags-mode)
    ;; (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
    )
  (add-hook 'c-mode-hook #'chunyang--setup-ggtags)
  (add-hook 'tcl-mode-hook #'chunyang--setup-ggtags))


;;; Tcl
(defun helm-inferior-tcl-complete ()
  (interactive)
  (helm :sources (helm-build-sync-source "inferior tcl completion"
                   :candidates (ring-elements comint-input-ring))))

(add-hook 'inferior-tcl-mode-hook
          (lambda ()
            (define-key inferior-tcl-mode-map
              [remap completion-at-point] #'helm-inferior-tcl-complete)))


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
  :init (defvar git-messenger-map nil)
  :bind ("C-x v P" . git-messenger:popup-message))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind ("C-x v t" . git-timemachine))

;;; emacs vc-mode & svn
;;  - [[http://lifegoo.pluskid.org/wiki/EmacsSubversion.html][Emacs 配合 Subversion 使用]]
;;  - [[http://lifegoo.pluskid.org/wiki/EmacsVC.html][EmacsVC]]

(use-package psvn :ensure t :defer t)

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "C")


;;; Tools and utilities
(use-package server
  :defer 7
  :config
  (unless (server-running-p) (server-start)))

(use-package helm-open-github :ensure t :defer t)

(use-package helm-github-stars
  :ensure t
  :config
  (load-file "~/.private.el")
  (add-hook 'helm-github-stars-clone-done-hook #'dired)
  (setq helm-github-stars-refetch-time (/ 6.0 24)
        helm-github-stars-full-frame t
        helm-github-stars-default-sources '(hgs/helm-c-source-stars
                                            hgs/helm-c-source-repos)))

(use-package helm-chrome ;; :ensure t :defer t
  :load-path "~/wip/helm-chrome/"
  :commands helm-chrome-bookmarks)

(use-package helm-firefox
  :ensure t :defer t
  :config (setq helm-firefox-default-directory
                "~/Library/Application Support/Firefox/"))

(use-package jist                       ; Gist
  :ensure t
  :commands jist-list
  :config (load-file "~/.private.el"))

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
          "C-c p"                       ; Project
          "C-c t"                       ; Personal Toggle commands
          "C-c L"                       ; Personal List something commands
          "C-c f"                       ; File
          "C-x v"                       ; VCS
          "C-c A"                       ; Align
          "C-c g"                       ; Google Search
          ))
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))
  (guide-key-mode))

(use-package keyfreq
  :disabled t
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(use-package hydra            :ensure t :defer t :disabled t)
(use-package dash-at-point    :ensure t :defer t)
(use-package helm-dash        :ensure t :defer t)

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  (use-package helm-projectile
    :ensure t
    :init (helm-projectile-on))
  :config (setq projectile-completion-system 'helm
                projectile-mode-line '(:eval
                                       (format " P[%s]"
                                               (projectile-project-name)))))


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
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-refile-folder "/[Gmail].All Mail")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)
  ;; skip duplicate messages (caused by the combination of Gmail and offlineimap)
  (setq mu4e-headers-skip-duplicates t)
  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"               . ?i)
           ("/[Gmail].All Mail"    . ?a)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Trash"       . ?t)))
  ;; Don't use ido to choose other Mail folder
  (setq mu4e-completing-read-function #'completing-read)
  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "proxychains4 offlineimap"
        mu4e-update-interval (* 15 60)  ; update every 15 minutes
        )

  (setq mu4e-user-mailing-lists
        '(("macports-dev.lists.macosforge.org"     . "MPDev")
          ("macports-users.lists.macosforge.org"   . "MPUser")
          ("macports-tickets.lists.macosforge.org" . "MPTicks")
          ("emacs-china.googlegroups.com"          . "EmacsCN")))

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
        "Chunyang Xu\n")

  (setq chunyang-main-mail-address      "xuchunyang56@gmail.com"
        chunyang-macports-mail-address  "chunyang@macports.org")
  ;; Let mu4e know these are mine
  (setq mu4e-user-mail-address-list `(,chunyang-main-mail-address
                                      ,chunyang-macports-mail-address))

  ;; use `chunyang-macports-mail-address' in some cases
  (defun auto-set-from-address ()
    (let* ((msg mu4e-compose-parent-message))
      (setq user-mail-address
            (if (seq-contains-p
                 (mapcar
                  (lambda (mail-address)
                    (if (or (mu4e-message-contact-field-matches msg :to  mail-address)
                            (mu4e-message-contact-field-matches msg :cc  mail-address))
                        t nil))
                  `(,chunyang-macports-mail-address
                    "macports-dev@lists.macosforge.org"
                    "macports-users@lists.macosforge.org"))
                 t)
                chunyang-macports-mail-address
              chunyang-main-mail-address))))
  (add-hook 'mu4e-compose-pre-hook #'auto-set-from-address)

  ;; Send via msmtp (for socks proxy support)
  (setq message-sendmail-f-is-evil 't)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments (list '"-a" "default"))
  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (use-package mu4e-maildirs-extension  ; Show maildirs summary in mu4e-main-view
    :ensure t
    :defer t
    :init (mu4e-maildirs-extension)))

(use-package helm-mu
  :ensure t
  :defer t
  :config (setq helm-mu-gnu-sed-program "gsed"
                helm-mu-skip-duplicates t))

(use-package sx                  :ensure t :defer t)
(use-package helm-zhihu-daily    :ensure t :defer t)

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :preface (defvar google-this-keybind (kbd "C-c g"))
  :init (google-this-mode))

(use-package elfeed :ensure t :defer t)


;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search-at-point)
         ("C-c Y" . youdao-dictionary-search-at-point+))
  :config
  (setq url-automatic-caching t)
  (push "*Youdao Dictionary*" popwin:special-display-config))

(use-package translate-shell
  :load-path "~/wip/translate-shell.el"
  :bind (("C-c s"   . translate-shell-brief)
         ("C-c S"   . translate-shell))
  :config
  ;; <https://translate.google.com> is blocked in China for no apparent
  ;; reason. No one ever asked my option.
  (setq translate-shell-command "proxychains4 -q trans -t en %s"
        translate-shell-brief-command "proxychains4 -q trans -brief -t zh %s"))

(use-package osx-dictionary
  :ensure t
  :bind (("C-c d" . osx-dictionary-search-pointer))
  :config
  (push "*osx-dictionary*" popwin:special-display-config))

(use-package bing-dict :ensure t :defer t)


;;; Web Development
(use-package restclient :ensure t :defer t)


;;; org-mode
(use-package org
  :bind (("C-c a"   . org-agenda)
         ("C-c c"   . org-capture)
         ("C-c l"   . org-store-link)
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
        '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
          ("e" "Emacs-related tasks" tags-todo "+emacs")
          ("g" "GSoC-related tasks" tags-todo "+gsoc")))

  (setq org-enforce-todo-dependencies t)

  (setq org-log-done 'time)

  ;; Clock work time
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)

  (use-package org-mac-link
    :if (eq system-type 'darwin)
    :ensure t
    :commands (org-mac-firefox-insert-frontmost-url
               org-mac-chrome-insert-frontmost-url))

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
  :init (global-orglink-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package calfw
  :ensure t :defer t
  :init (use-package calfw-org :commands cfw:open-org-calendar))

(bind-key "C-h h" #'describe-personal-keybindings)
(bind-key "C-h C-k" #'find-function-on-key)

;;; init.el ends here
