;;; init.el --- Chunyang Xu's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Debugging
(setq message-log-max 10000)


;;; Start up

;; Don't load outdated byte code
(setq load-prefer-newer t)

;; OK, redefinition is fine for me
(setq ad-redefinition-action 'accept)

(require 'package)
;; FIXME: check this option
;; (setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; My private packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; Require helper libraries

(require 'subr-x)
(require 'rx)
(use-package dash :ensure t)

;; http://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))


;;; Initialization

;; And disable the site default settings (TODO: but what is it exactly?)
(setq inhibit-default-init t)


;; Load personal information
(load "~/.private.el" :no-error)


;;; Customization

(use-package cus-edit
  :defer t
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error :no-message))


;;; OS X support
(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil ; Don't pop up new frames from the workspace
        mac-command-modifier 'meta
        mac-option-modifier 'control))

(use-package exec-path-from-shell       ; TODO: Try /etc/paths instead
  :disabled t
  :ensure t
  :if (and (eq system-type 'darwin) (display-graphic-p))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-copy-env "INFOPATH")
  (exec-path-from-shell-initialize))


;;; User Interface
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(setq ring-bell-function #'ignore)      ; Don't ring bell on C-g

(fset 'yes-or-no-p #'y-or-n-p)

;; Font
(set-face-attribute 'default nil
                    :font "Source Code Pro-13")

;; Theme
(setq custom-safe-themes t)

(use-package spacemacs-theme
  :defer t
  :load-path "~/wip/spacemacs-theme"
  :init
  (add-to-list 'custom-theme-load-path "~/wip/spacemacs-theme")
  (setq spacemacs-theme-comment-bg nil
        spacemacs-theme-org-height nil
        spacemacs-theme-org-highlight nil)
  (load-theme 'spacemacs-dark :no-confirm))

;; Mode line
(defvar chunyang-mode-line-format mode-line-format
  "Original value of `mode-line-format'.")

(defun chunyang-revert-modeline ()
  "Revert to the default Emacs mode-line."
  (interactive)
  (setq-default mode-line-format chunyang-mode-line-format))

(setq-default mode-line-format
              '("%e" mode-line-front-space
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification "    " mode-line-position
                ;; For debug some elisp program
                ;; (:eval (format "%-5d" (point)))
                (projectile-mode projectile-mode-line)
                (vc-mode (:propertize (:eval vc-mode) face italic))
                ;; Other modes
                " " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(column-number-mode)
(size-indication-mode)

(use-package spaceline
  :disabled t
  :load-path "~/wip/spaceline"
  :defer t
  :init
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  :config
  (setq powerline-default-separator 'wave)
  (setq spaceline-workspace-numbers-unicode t))

;; Echo Area
(setq echo-keystrokes 0.6)


;;; Emacs session persistence
(use-package desktop                    ; frame/window/buffer and global vars
  :config
  (add-to-list 'desktop-globals-to-save 'initial-scratch-message)
  (desktop-save-mode)

  (add-hook 'kill-emacs-hook
            (lambda ()
              (with-current-buffer "*scratch*"
                (setq initial-scratch-message
                      (buffer-string)))))

  (add-hook 'emacs-startup-hook      ; Assuming it runs after *scratch* is ready
            (lambda ()
              "Fix 标点符号 caused by `substitute-command-keys'."
              (-when-let (buffer (get-buffer "*scratch*"))
                (goto-char (point-min))
                (replace-string "’" "'")))))

(use-package savehist                   ; Minibuffer history
  :init (savehist-mode)
  :config
  (setq history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(extended-command-history)))

(use-package recentf                    ; Recent files
  :defer t
  :config
  (setq recentf-max-saved-items 200
        ;; Cleanup recent files only when Emacs is idle
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              "/itsalltext/"  ; It's all text temp files
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'")))

(use-package bookmark
  :defer t
  :config (setq bookmark-save-flag 1))

(use-package saveplace                  ; Save point position in files
  :defer t
  :init (add-hook #'after-init-hook #'save-place-mode))


;;; Minibuffer with helm

(defvar chunyang-prefer-helm-p t)

(when chunyang-prefer-helm-p
  (require 'chunyang-helm))

(use-package swiper
  :if (not chunyang-prefer-helm-p)
  :ensure t
  :bind (("C-z"    .  ivy-resume)
         ("C-s"    .  swiper)
         ("M-l"    .  ivy-switch-buffer)
         ("C-x f"  .  ivy-recentf))
  :config
  (ivy-mode)
  (diminish 'ivy-mode)
  (bind-key "C-o" 'imenu))

(use-package counsel
  :if (not chunyang-prefer-helm-p)
  :ensure t
  :bind ("M-x" . counsel-M-x))


;;; Buffers, Windows and Frames

(use-package uniquify            ; Make buffer names unique (turn on by default)
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode))

(use-package chunyang-simple
  :bind (("C-x 3" . chunyang-split-window-right)
         ("C-x 2" . chunyang-split-window-below)
         ("C-h t" . chunyang-switch-scratch)))

(use-package chunyang-buffers          ; Personal buffer tools
  :commands (lunaryorn-do-not-kill-important-buffers)
  :init (add-hook 'kill-buffer-query-functions
                  #'lunaryorn-do-not-kill-important-buffers))

(bind-key "O" #'delete-other-windows  special-mode-map)
(bind-key "Q" #'kill-this-buffer      special-mode-map)

(use-package ace-window
  :ensure t
  :preface
  (defun chunyang-ace-window (arg)
    "A modified version of `ace-window'.
     When number of window <= 3, invoke `other-window', otherwise `ace-window'.
     One C-u, swap window, two C-u, delete window."
    (interactive "p")
    (cl-case arg
      (0
       (setq aw-ignore-on
             (not aw-ignore-on))
       (ace-select-window))
      (4 (ace-swap-window))
      (16 (ace-delete-window))
      (t (if (<= (length (window-list)) 3)
             (other-window 1)
           (ace-select-window)))))
  :config
  (bind-key "M-o" #'chunyang-ace-window)
  (setq aw-ignore-current t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode))

(use-package eyebrowse
  :ensure t
  :config (eyebrowse-mode))

(use-package shackle
  :ensure t
  :disabled t
  :init (shackle-mode)
  :config (setq shackle-rules '(("\\‘\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4))))

;; Frames
(setq frame-resize-pixelwise t          ; Resize by pixels
      frame-title-format
      '(:eval (if (buffer-file-name)
                  (abbreviate-file-name (buffer-file-name)) "%b")))

(use-package frame
  :bind (("C-c t F" . toggle-frame-fullscreen)
         ("C-c t m" . toggle-frame-maximized))
  :config
  ;; (add-to-list 'initial-frame-alist '(maximized . fullscreen))
  (unbind-key "C-x C-z"))


;;; File handle
;; Keep backup and auto save files out of the way
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files
  :bind (("C-c f u" . revert-buffer)
         ("C-c f n" . normal-mode))
  :config
  ;; Prefer GNU variants
  (setq insert-directory-program "gls"
        find-program "gfind"
        grep-program "grep"))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (setq dired-listing-switches "-alh")
  (use-package dired-x
    :commands dired-omit-mode
    :init (add-hook 'dired-mode-hook #'dired-omit-mode)))

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)


;;; Basic Editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Indicate empty lines at the end of a buffer in the fringe, but require a
;; final new line
(setq indicate-empty-lines t
      require-final-newline t)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(use-package electric                   ; Electric code layout
  :init (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :init (electric-pair-mode))

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)

;; I prefer indent long-line code myself
(setq comment-auto-fill-only-comments t)

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

(diminish 'auto-fill-function)          ; Not `auto-fill-mode' as usual

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char)
         ("M-g l" . avy-goto-line))
  :config
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-'") #'avy-isearch)))

(use-package pin)

(use-package ace-link
  :ensure t
  :init (ace-link-setup-default))

(use-package easy-kill                  ; Easy killing and marking on C-w
  :disabled t
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill) ; M-w
         ([remap mark-sexp]      . easy-mark) ; C-M-SPC
         ))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :ensure t
  :bind (("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o n"     . mc/mark-next-like-this)
         ("C-c o p"     . mc/mark-previous-like-this)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o C-s"   . mc/mark-all-in-region)))

(use-package undo-tree                  ; Branching undo
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package nlinum                     ; Line numbers in display margin
  :ensure t
  :bind (("C-c t l" . nlinum-mode)))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'view-hello-file
     'disabled "I mistype C-h h a lot and it is too slow to block Emacs")


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun view-help-buffer ()
  "View the `*Help*' buffer."
  (interactive)
  (display-buffer (help-buffer)))

(bind-key "C-h h" #'view-help-buffer)


;;; Navigation and scrolling

;; (setq scroll-preserve-screen-position 'always)

(setq scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      scroll-conservatively 10          ; Smooth Scrolling
      )

;; These settings make trackpad scrolling on OS X much more predictable
;; and smooth
(setq mouse-wheel-progressive-speed nil
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
  ;; Helper function
  (defun my-imenu--build-expression (name)
    "Return `imenu-generic-expression' of macro or function NAME."
    (list
     name (rx-to-string
           `(and ,(concat "(" name)
                 symbol-end (1+ (syntax whitespace)) symbol-start
                 (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                 symbol-end)) 1))

  (defun my-imenu--setup-elisp ()
    (dolist (name '("use-package" "defhydra"))
      (add-to-list 'imenu-generic-expression (my-imenu--build-expression name))))

  (add-hook 'emacs-lisp-mode-hook #'my-imenu--setup-elisp))


;;; Search
(setq isearch-allow-scroll t)

(use-package grep
  :defer t
  :config
  (dolist (file '("TAGS" "GPATH" "GRTAGS" "GTAGS"))
    (add-to-list 'grep-find-ignored-files file))
  (add-to-list 'grep-find-ignored-directories "auto")
  (add-to-list 'grep-find-ignored-directories "elpa")
  (use-package wgrep :ensure t :defer t))

(use-package anzu                       ; Position/matches count for isearch
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :config
  :config
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp))

(use-package region-state
  :load-path "~/wip/region-state.el/"
  :config (region-state-mode))

(use-package pinyin-search
  :ensure t
  :defer t)


;;; Highlight
(use-package whitespace                 ; Highlight bad whitespace (tab)
  :bind ("C-c t w" . whitespace-mode)
  :config
  (setq whitespace-style '(face indentation space-after-tab space-before-tab
                                tab-mark empty trailing lines-tail)
        whitespace-line-column nil)
  :diminish whitespace-mode)

(use-package hl-line
  :bind ("C-c t L" . hl-line-mode))

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
  ;; Need to save my eyes
  ;; :init (add-hook 'after-init-hook #'global-color-identifiers-mode)
  )

(use-package highlight-numbers          ; Fontify number literals
  :disabled t
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol           ; Highlighting and commands for symbols
  :ensure t
  :diminish highlight-symbol-mode
  :init
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  ;; Highlight symbol occurrences
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-on-navigation-p t))

(use-package rainbow-mode               ; Fontify color values in code
  :ensure t
  :diminish rainbow-mode
  :config (add-hook 'css-mode-hook #'rainbow-mode))


;;; Skeletons, completion and expansion
(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ("M-/" . hippie-expand)
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

(use-package yasnippet
  :disabled t
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode))


;;; Spelling and syntax checking

(use-package flyspell
  :diminish flyspell-mode
  :bind ("C-c t s" . flyspell-mode)
  ;; :init
  ;; (use-package ispell
  ;;   :config (setq ispell-program-name "aspell"
  ;;                 ispell-extra-args '("--sug-mode=ultra")))
  ;; (add-hook 'text-mode-hook #'flyspell-mode)
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (unbind-key "C-." flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-;" flyspell-mode-map)
  (use-package flyspell-popup
    :load-path "~/wip/flyspell-popup"
    :config
    (bind-key "C-." #'flyspell-popup-correct flyspell-mode-map)
    (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)))

(use-package checkdoc
  :config (setq checkdoc-arguments-in-order-flag nil
                checkdoc-force-docstrings-flag nil))

(use-package flycheck
  :disabled t
  :ensure t
  :bind (("C-c t f" . global-flycheck-mode)
         ("C-c L e" . list-flycheck-errors))
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  (use-package flycheck-pos-tip           ; Show Flycheck messages in popups
    :ensure t
    :config (setq flycheck-display-errors-function
                  #'flycheck-pos-tip-error-messages))

  (use-package flycheck-color-mode-line
    :disabled t
    :ensure t
    :config
    (eval-after-load "flycheck"
      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))


;;; Markup languages

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :config (setq markdown-command "kramdown"))

(use-package yaml-mode :ensure t :defer t)


;;; Programming utilities
(use-package compile
  :disabled t
  :bind (("C-c C" . compile))
  :preface
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

(use-package quickrun
  :ensure t :defer t)

(use-package prog-mode
  :bind ("C-c t p" . prettify-symbols-mode)
  :init (global-prettify-symbols-mode))


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
  (unbind-key "M-?" paredit-mode-map)

  (use-package paredit-menu
    :ensure t
    :commands menubar-paredit))


;;; Emacs Lisp
(use-package lisp-mode
  :defer t
  :preface
  ;; (defadvice pp-display-expression (after make-read-only (expression out-buffer-name) activate)
  ;;   "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
  ;;   (when (get-buffer out-buffer-name)
  ;;     (with-current-buffer out-buffer-name
  ;;       (view-mode))))

  (defun chunyang-elisp-function-or-variable-quickhelp (symbol)
    "Display a short documentation of function or variable using `popup'.

See also `describe-function-or-variable'."
    (interactive
     (let* ((v-or-f (variable-at-point))
            (found (symbolp v-or-f))
            (v-or-f (if found v-or-f (function-called-at-point))))
       (list v-or-f)))
    (if (not (and symbol (symbolp symbol)))
        (message "You didn't specify a function or variable")
      (let* ((fdoc (when (fboundp symbol)
                     (or (documentation symbol t) "Not documented.")))
             (fdoc-short (and (stringp fdoc)
                              (substring fdoc 0 (string-match "\n" fdoc))))
             (vdoc (when  (boundp symbol)
                     (or (documentation-property symbol 'variable-documentation t)
                         "Not documented as a variable.")))
             (vdoc-short (and (stringp vdoc)
                              (substring vdoc 0 (string-match "\n" vdoc)))))
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
    :disabled t
    :ensure t
    :diminish rebox-mode
    :bind ("M-q" . rebox-dwim)
    :preface
    (defun chunyang--elisp-comment-setup ()
      (setq-local rebox-style-loop '(21 23 25 27))
      (setq-local rebox-min-fill-column 40))
    :config
    (add-hook 'emacs-lisp-mode-hook #'chunyang--elisp-comment-setup))

  ;; TODO make my own hook func
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'ipretty-mode)
  ;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  ;;   (add-hook hook 'turn-on-elisp-slime-nav-mode))
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package chunyang-elisp
  :config
  (bind-key "C-M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

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


;;; Help

(bind-key "C-h C-k" #'find-function-on-key)


;;; Version Control
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-revert-buffers t))

(use-package git-gutter
  :disabled t
  :ensure t
  :diminish git-gutter-mode
  :bind (("C-x C-g" . git-gutter:toggle)
         ("C-x v n" . git-gutter:next-hunk)
         ("C-x v p" . git-gutter:previous-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk))
  :init
  (setq git-gutter:handled-backends '(git svn))
  (global-git-gutter-mode))

(use-package diff-hl
  :disabled t
  :ensure t
  :init (global-diff-hl-mode))

(use-package git-messenger
  :ensure t
  :bind ("C-x v P" . git-messenger:popup-message))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package gitconfig-mode             ; Edit .gitconfig files
  :ensure t
  :defer t)

(use-package gitignore-mode             ; Edit .gitignore files
  :ensure t
  :defer t)


;;; Tools and utilities
(use-package edit-server
  :ensure t
  :defer 10
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)
  (setq ediff-custom-diff-program "diff"
        ediff-custom-diff-options "-u"))

(use-package server
  :defer 7
  :config
  (unless (server-running-p) (server-start)))

(use-package gh-md             :ensure t :defer t)

(use-package github-notifier
  :load-path "~/wip/github-notifier.el/"
  :commands github-notifier-mode)

(use-package which-key
  :disabled t
  :ensure t
  :config (which-key-mode))

(use-package fcitx
  :disabled t
  :load-path "~/wip/fcitx.el"
  :commands (fcitx-default-setup fcitx-aggressive-setup)
  :init
  ;; (fcitx-default-setup)
  (fcitx-aggressive-setup))


;;; Project
(use-package projectile
  :disabled t
  :load-path "~/wip/projectile"
  :commands projectile-global-mode
  :diminish projectile-mode
  :init (projectile-global-mode)
  :config
  (setq projectile-completion-system 'helm
        projectile-mode-line '(" P["
                               (:propertize (:eval (projectile-project-name))
                                            face bold)
                               "]"))
  (use-package helm-projectile
    :init (helm-projectile-on)))


;;; Web & IRC & Email & RSS
(use-package mu4e
  :load-path "/opt/local/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  ;; Setup
  (setq mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash"
        mu4e-refile-folder "/[Gmail].All Mail")

  (setq mu4e-headers-skip-duplicates t)

  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  ;; Fetch - Read new mail when I'm ready.
  ;; updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "proxychains4 offlineimap")

  ;; Read
  (setq mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
          ("date:today..now"                  "Today's messages"     ?t)
          ("date:7d..now"                     "Last 7 days"          ?w))
        mu4e-maildir-shortcuts
        '( ("/INBOX"               . ?i)
           ("/[Gmail].Sent Mail"   . ?s)
           ("/[Gmail].Trash"       . ?t)
           ("/[Gmail].All Mail"    . ?a)))

  ;; show images
  (setq mu4e-view-show-images t)

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

  ;; Write
  ;; spell check
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))

  (setq mu4e-compose-signature "Chunyang Xu")

  ;; Send via msmtp (for socks proxy support)
  (setq message-sendmail-f-is-evil 't)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp")
  (setq message-sendmail-extra-arguments (list "-a" "default"))

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; org-mode support
  (require 'org-mu4e)
  (use-package mu4e-maildirs-extension  ; Show maildirs summary in mu4e-main-view
    :disabled t
    :ensure t
    :init (mu4e-maildirs-extension)))

(use-package erc
  :config
  ;; Join the #emacs channels whenever connecting to Freenode.
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))

  ;; Shorten buffer name (e.g., "freenode" instead of "irc.freenode.net:6667")
  (setq erc-rename-buffers t)

  (defun chat ()
    "Chat in IRC with ERC."
    (interactive)
    (erc :server "irc.freenode.net"
         :port "6667"
         :nick erc-nick
         :password erc-password)))

(use-package sx                  :ensure t :defer t)

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :preface (defvar google-this-keybind (kbd "C-c G"))
  :init (google-this-mode))


;;; Dictionary
(use-package youdao-dictionary
  :load-path "~/wip/youdao-dictionary"
  :bind (("C-c y" . youdao-dictionary-search)
         ("C-c Y" . youdao-dictionary-search-at-point+))
  ;; :config (setq url-cache-expire-time (* 60 60 24) ; 24 hours
  ;;               url-automatic-caching t)
  )

(use-package translate-shell
  :disabled t
  :load-path "~/wip/translate-shell.el"
  :bind (("C-c s"   . translate-shell-brief)
         ("C-c S"   . translate-shell))
  :config
  ;; <https://translate.google.com> is blocked in China for no apparent
  ;; reason. No one ever asked my option.
  (setq translate-shell-command "proxychains4 -q trans -t en %s"
        translate-shell-brief-command "proxychains4 -q trans -brief -t zh %s"))

(use-package osx-dictionary
  :load-path "~/wip/osx-dictionary.el"
  :bind ("C-c d" . osx-dictionary-search-pointer))

(use-package bing-dict
  :ensure t
  :preface
  (defun say ()
    (interactive)
    (let ((text (if (use-region-p)
                    (buffer-substring (region-beginning) (region-end))
                  (thing-at-point 'word))))
      (when text
        (start-process "say" nil "say" text))))
  :config
  ;; (defvar bing-dict-query-word-at-point-timer
  ;;   (run-with-idle-timer 2.1 t
  ;;                        (lambda ()
  ;;                          (let ((word (thing-at-point 'word)))
  ;;                            (when (and word (> (length word) 3) (< (length word) 21)
  ;;                                       (not (minibufferp)))
  ;;                              ;; (my-log "%s" word)
  ;;                              (bing-dict-brief word))))))
  ;; (defun bing-dict-stop-timer ()
  ;;   (interactive)
  ;;   (cancel-timer bing-dict-query-word-at-point-timer)
  ;;   (message "bing-dict-query-word-at-point-timer canceled"))
  )

(defvar google-translate-history nil)

(defun google-translate (query)
  (interactive
   (let* ((initial-input
           (when (use-region-p)
             (buffer-substring (region-beginning) (region-end))))
          (string (read-string "Translate: "
                               initial-input
                               'google-translate-history)))
     (list string)))
  (browse-url (format "https://www.google.com/translate_t?text=%s"
                      (url-hexify-string query))))


;;; Eshell

(use-package eshell
  :preface
  (defun eshell* ()
    "Start a new eshell even if one is active."
    (interactive)
    (eshell t))
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (defun eshell/mcd (dir)
    "make a directory and cd into it"
    (eshell/mkdir "-p" dir)
    (eshell/cd dir))
  (defun eshell-insert-last-arg ()
    "Insert the last arg of the last command, like ESC-. in shell."
    (interactive)
    (with-current-buffer "*eshell*"
      (let ((last-arg
             (car (last
                   (split-string
                    (substring-no-properties (eshell-get-history 0)))))))
        (when last-arg
          (insert last-arg)))))
  :bind  (("C-!"   . eshell-command)
          ("C-x m" . eshell)
          ("C-x M" . eshell*))
  :config
  (setq eshell-history-size 5000)       ; Same as $HISTSIZE
  (setq eshell-hist-ignoredups t)       ; make the input history more bash-like
  (setq eshell-banner-message
        '(concat (shell-command-to-string "fortune") "\n"))
  ;; needed at least for `eshell-git-prompt'
  (setq eshell-highlight-prompt nil)

  (defun eshell/x ()
    (insert "exit")
    (eshell-send-input)
    (delete-window))
                                        ; (I don't know what this means)
  (add-hook 'eshell-mode-hook
            (lambda ()
              ;; Setup smart shell
              ;; (require 'em-smart)
              ;; (eshell-smart-initialize)
              (bind-keys :map eshell-mode-map
                         ;; ("TAB"     . helm-esh-pcomplete)
                         ;; ("M-p"     . helm-eshell-history)
                         ;; ("C-l"     . eshell-clear-buffer)
                         ("C-c C-k" . compile)
                         ("C-c C-q" . eshell-kill-process)
                         ("C-c ."   . eshell-insert-last-arg))
              (eshell/export "EDITOR=emacsclient -n")
              (eshell/export "VISUAL=emacsclient -n")))

  ;; Eshell command name completion for tldr man pages <http://tldr-pages.github.io>
  (defvar tldr-commands nil)

  (defun pcomplete/tldr ()
    (unless tldr-commands
      (setq tldr-commands
            (split-string
             (nth 1 (split-string
                     (shell-command-to-string "tldr --list")
                     "\n" t))
             ", ")))
    (pcomplete-here* tldr-commands))

  (use-package eshell-git-prompt
    :load-path "~/wip/eshell-git-prompt"
    :config (eshell-git-prompt-use-theme 'powerline))

  (use-package eshell-z
    :load-path "~/wip/eshell-z"))

(use-package eshell-did-you-mean
  :load-path "~/wip/eshell-did-you-mean"
  :config (eshell-did-you-mean-setup))


;;; Org mode

;;; Install org from Git, since I have trouble to access http://orgmode.org/elpa/
(add-to-list 'load-path "~/wip/org-mode/lisp")
;; Various org-mode extensions
(add-to-list 'load-path "~/wip/org-mode/contrib/lisp" :append)

(use-package org
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
    (if (org-clocking-p)
        (with-temp-buffer
          (insert org-mode-line-string)
          (buffer-substring-no-properties (point-min) (point-max)))
      "[无所事事]")))

(use-package org-mac-link
  :if (eq system-type 'darwin)
  :ensure t
  :commands (;; org-mac-firefox-insert-frontmost-url
             org-mac-safari-insert-frontmost-url
             org-mac-chrome-insert-frontmost-url))

(use-package orglink
  :diminish orglink-mode
  :config (global-orglink-mode))

(use-package org-bullets
  :disabled t
  :config (add-hook 'org-mode-hook #'org-bullets-mode))


;;; Emacs Development

(setq tags-table-list '("~/wip/emacs"))


;;; Common Lisp

(use-package slime
  :ensure t
  :defer t
  :config
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

;;; init.el ends here
