;;; init.el --- Emacs configuration of Chunyang Xu  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015 Chunyang Xu <xu.chunyang@icloud.com>
;;
;; Author: Chunyang Xu <xu.chunyang@icloud.com>
;; URL: https://github.com/xuchunyang/emacs.d
;;
;;; License: GPLv3

;;; Commentary:

;; User key prefixes:
;;
;; - C-c A: Align
;; - C-c h: Helm (not using now)
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
  (defvar use-package-verbose nil)
  ;; (defvar use-package-expand-minimally t)
  (eval-after-load 'advice
    `(setq ad-redefinition-action 'accept))
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(require 'subr-x)
(require 'rx)

(bind-key "C-c L p" #'package-list-packages)
(bind-key "C-c L P" #'package-list-packages-no-fetch)

;; My personal packages
(push (expand-file-name "lisp" user-emacs-directory) load-path)


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


;; Font setup
(set-frame-font "Source Code Pro-13" nil t)   ; Default font

;; Additional fonts for special characters and fallbacks
;; Test range: 🐷 ⊄ ∫ 𝛼 α 🜚
(when (eq system-type 'darwin)
  ;; Colored Emoji on OS X
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                    nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Apple Symbols") nil 'append))
(set-fontset-font t 'mathematical (font-spec :family "XITS Math") nil 'append)
;; Fallback for Greek characters which Source Code Pro doesn't provide.
(set-fontset-font t 'greek (pcase system-type
                             (`darwin (font-spec :family "Menlo"))
                             (_ (font-spec :family "DejaVu Sans Mono")))
                  nil 'append)

;; A general fallback for all kinds of unknown symbols
(set-fontset-font t nil (font-spec :family "Apple Symbols") nil 'append)

;;; TODO Chinese font setup
;; (when (member "STFangsong" (font-family-list))
;;   (set-fontset-font t 'han (font-spec :family "STFangsong"))
;;   (setq face-font-rescale-alist '(("STFangsong" . 1.3))))

(use-package zenburn-theme                     :ensure t :defer t)

(use-package solarized-theme
  :disabled t
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow    :ensure t :defer t)
(use-package spacemacs-theme                   :ensure t :defer t)


;;; The mode line
(setq-default mode-line-format
              '("%e" mode-line-front-space
                "👿 "
                ;; Standard info about the current buffer
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification " " mode-line-position
                (projectile-mode projectile-mode-line)
                (vc-mode (:propertize (:eval vc-mode) face italic))
                " "
                (flycheck-mode flycheck-mode-line) ; Flycheck status
                (isearch-mode " ")
                (anzu-mode (:eval                  ; isearch pos/matches
                            (when (> anzu--total-matched 0)
                              (anzu--update-mode-line))))
                ;; (multiple-cursors-mode mc/mode-line) ; Number of cursors
                ;; And the modes, which we don't really care for anyway
                " " mode-line-misc-info mode-line-modes mode-line-end-spaces)
              mode-line-remote
              '(:eval
                (when-let (host (file-remote-p default-directory 'host))
                  (propertize (concat "@" host) 'face
                              '(italic warning))))
              ;; Remove which func from the mode line, since we have it in the
              ;; header line
              mode-line-misc-info
              (assq-delete-all 'which-func-mode mode-line-misc-info)

              ;; header-line-format
              ;; '(which-func-mode ("" which-func-format " "))
              )

(use-package powerline
  :disabled t
  :ensure t
  :config
  (setq powerline-display-mule-info nil
        powerline-display-buffer-size t)
  :init
  (powerline-default-theme))

(column-number-mode)

(use-package smart-mode-line
  :disabled t
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  ;; (setq sml/theme 'respectful)
  (sml/setup))

(use-package telephone-line
  :load-path "~/wip/telephone-line"
  :commands (telephone-line-enable telephone-line-disable)
  :config
  (setq telephone-line-lhs
        '((accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (nil    . (telephone-line-minor-mode-segment
                     telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((accent . (telephone-line-major-mode-segment))
          (nil    . (telephone-line-misc-info-segment)))))

(use-package nyan-mode
  :disabled t
  :ensure t
  :config
  (nyan-mode))


;;; The minibuffer

(defvar prefer-helm t)
(defvar prefer-ivy (not prefer-helm))

(use-package helm
  :if prefer-helm
  :ensure t
  :config
  (setq helm-split-window-default-side 'other))

(use-package helm-config
  :if prefer-helm
  :init
  (defvar helm-command-prefix-key "C-c h")
  :config
  (bind-keys :map helm-command-map
             ("g"   . helm-chrome-bookmarks)
             ("z"   . helm-complex-command-history)
             ("C-/" . helm-fuzzy-find)
             ("G"   . helm-github-stars))
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
  :if prefer-helm
  :diminish helm-mode
  :init (helm-mode))

(when prefer-helm
  (require 'helm-misc)
  (require 'helm-command)
  (require 'helm-imenu)
  (require 'helm-semantic)
  (require 'helm-ring))

(use-package helm-adaptive
  :if prefer-helm
  :config (helm-adaptive-mode))

(use-package helm-regexp
  :if prefer-helm
  :defer t)

(use-package helm-command
  :if prefer-helm
  :defer t
  :config (setq helm-M-x-always-save-history t))

(use-package wgrep-helm :ensure t)

(use-package helm-buffers
  :if prefer-helm
  :defer t
  :config
  (add-to-list 'helm-boring-buffer-regexp-list "TAGS")
  (add-to-list 'helm-boring-buffer-regexp-list "git-gutter:diff")

  (defun helm-buffer-switch-to-new-window (_candidate)
    "Display buffers in new windows."
    ;; Select the bottom right window
    (require 'winner)
    (select-window (car (last (winner-sorted-window-list))))
    ;; Display buffers in new windows
    (dolist (buf (helm-marked-candidates))
      (select-window (split-window-right))
      (switch-to-buffer buf))
    ;; Adjust size of windows
    (balance-windows))

  (add-to-list 'helm-type-buffer-actions
               '("Display buffer(s) in new window(s) `M-o'" .
                 helm-buffer-switch-new-window) 'append)

  (defun helm-buffer-switch-new-window ()
    (interactive)
    (with-helm-alive-p
      (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window)))

  (define-key helm-buffer-map (kbd "M-o") #'helm-buffer-switch-new-window)

  (defun helm-buffer-imenu (candidate)
    "Imenu action for helm buffers."
    (switch-to-buffer candidate)
    ;; (call-interactively #'helm-imenu)
    (require 'helm-imenu)
    (unless helm-source-imenu
      (setq helm-source-imenu
            (helm-make-source "Imenu" 'helm-imenu-source
              :fuzzy-match helm-imenu-fuzzy-match)))
    (let ((imenu-auto-rescan t))
      ;; FIXME: can't execute action in nest helm session,
      ;; maybe something is special in `helm-source-imenu'.
      (helm :sources 'helm-source-imenu
            :buffer "*helm imenu*"
            :resume 'noresume
            :allow-nest t)))

  (add-to-list 'helm-type-buffer-actions
               '("Imenu" . helm-buffer-imenu) 'append))

(use-package helm-files
  :if prefer-helm
  :bind ("C-c p h" . helm-browse-project)
  :config
  ;; Add imenu action to 'C-x C-f'
  (defun helm-find-file-imenu (file)
    (helm-find-file-or-marked file)
    (call-interactively #'helm-imenu))

  (add-to-list 'helm-find-files-actions
               '("Imenu" . helm-find-file-imenu)
               'append)

  (add-to-list 'helm-boring-file-regexp-list ".DS_Store")
  (use-package helm-ls-git :ensure t :defer t)

  (use-package helm-ls-svn
    :load-path "~/wip/chunyang/helm-ls-svn.el"
    :bind ("M-8" . helm-ls-svn-ls))

  (use-package helm-fuzzy-find
    :load-path "~/wip/helm-fuzzy-find/"
    :commands helm-fuzzy-find))

(when prefer-helm
  (bind-keys ("M-x"                            . helm-M-x)
             ;; File
             ("C-x C-f"                        . helm-find-files)
             ("C-x f"                          . helm-recentf)
             ;; Buffer
             ([remap switch-to-buffer]         . helm-buffers-list)       ; C-x b
             ("M-l"                            . helm-mini)               ; M-l
             ;; Kill ring
             ([remap yank-pop]                 . helm-show-kill-ring)     ; M-y
             ("C-z"                            . helm-resume)
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
             ;; ("C-c <SPC>"                      . helm-all-mark-rings)
             ("M-i"                            . helm-occur)
             ("C-c i"                          . helm-semantic-or-imenu)))

;; `helm-regexp.el'
;; (bind-key "M-i" #'helm-occur-from-isearch isearch-mode-map)
;; `helm-misc.el'
;; (bind-key "C-c C-l"    #'helm-minibuffer-history    minibuffer-local-map)

(use-package helm-man
  :if prefer-helm
  :defer t
  :config (setq helm-man-format-switches "%s"))

(use-package helm-imenu
  :if prefer-helm
  :config
  ;; Re-define `helm-imenu-transformer' to support more colors
  (defvar helm-imenu-prop-alist
    '(("Variables" . font-lock-variable-name-face)
      ("Function"  . font-lock-function-name-face)
      ("Types"     . font-lock-type-face)
      ;; User defined
      ("Package"   . font-lock-keyword-face)
      ("hydra"     . font-lock-comment-face)))
  (defun helm-imenu-transformer (candidates)
    (cl-loop for (k . v) in candidates
             for types = (or (helm-imenu--get-prop k)
                             (list "Function" k))
             for bufname = (buffer-name (marker-buffer v))
             for disp1 = (mapconcat
                          (lambda (x)
                            (propertize
                             x 'face (catch 'break
                                       (dolist (elt helm-imenu-prop-alist)
                                         (when (string-equal x (car elt))
                                           (throw 'break (cdr elt)))))))
                          types helm-imenu-delimiter)
             for disp = (propertize disp1 'help-echo bufname)
             collect
             (cons disp (cons k v)))))

(use-package helm-ag
  :ensure t
  :bind (("C-c s" . helm-do-ag) ; C-u chooses file type, C-- enter your own cmd
                                ; options
         ("C-c S" . helm-do-ag-project-root)))

(use-package helm-descbinds
  :disabled t
  :load-path "~/wip/helm-descbinds"
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

(use-package swiper
  :if prefer-ivy
  :load-path "~/wip/swiper"
  :bind ("C-z" . ivy-resume)
  :config
  ;; Type `C-u C-j' or `C-M-j' or `C-RET' to use entered text and exit
  (bind-key "<C-return>" #'ivy-immediate-done ivy-minibuffer-map)

  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")

  ;; [[https://github.com/abo-abo/swiper/wiki/Customize-candidate-menu-style][Customize candidate menu style · abo-abo/swiper Wiki]]
  (setq ivy-format-function 'eh-ivy-format-function)
  (defun eh-ivy-format-function (cands)
    (let ((i -1))
      (mapconcat
       (lambda (s)
         (concat (if (eq (cl-incf i) ivy--index)
                     "👉 "
                   "   ")
                 s))
       cands "\n")))

  (use-package ivy
    ;; :diminish (ivy-mode . " 🙏")
    :config (ivy-mode)))

(use-package counsel
  :if prefer-ivy
  :load-path "~/wip/swiper"
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-l"     . ivy-switch-buffer)
         ("C-x f"   . ivy-recentf))
  :init (require 'counsel))

;;; Use these even using ivy
(bind-keys ("M-y" . helm-show-kill-ring)
           ("C-o" . helm-imenu)
           ("M-i" . helm-occur))
(setq helm-split-window-default-side 'other)

(defun chunyang-use-ivy ()
  (interactive)

  (setq prefer-helm nil
        prefer-ivy t)

  ;; Disable helm
  (helm-mode -1)
  (helm-projectile-off)
  (setq projectile-completion-system 'ivy)

  (use-package swiper
    :load-path "~/wip/swiper"
    :bind ("C-z" . ivy-resume)
    :config
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
    (ivy-mode))

  ;; Important missing function for me
  ;; 1. imehu
  ;; 2. kill-ring ('M-y')
  (bind-keys ("M-y" . helm-show-kill-ring)
             ("C-o" . helm-imenu)
             ("M-i" . helm-occur))

  (use-package counsel
    :load-path "~/wip/swiper"
    :bind (("M-x"     . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("M-l"     . ivy-switch-buffer)
           ("C-x f"   . ivy-recentf))))


(defun chunyang-use-helm ()
  (interactive)
  ;; Disable ivy
  (ivy-mode -1)
  (helm-mode)
  (helm-projectile-on)
  (setq projectile-completion-system 'helm)

  (setq prefer-helm t
        prefer-ivy nil)

  (load user-init-file))


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

(use-package chunyang-buffers          ; Personal buffer tools
  :load-path "lisp/"
  :commands (lunaryorn-do-not-kill-important-buffers)
  :init (add-hook 'kill-buffer-query-functions
                  #'lunaryorn-do-not-kill-important-buffers))

(use-package ibuffer                    ; Better buffer list
  :bind (([remap list-buffers] . ibuffer)))

(use-package windmove
  :disabled t
  :config
  (windmove-default-keybindings))

(use-package desktop                    ; Save buffers, windows and frames
  :init (desktop-save-mode)
  :config
  (add-to-list 'desktop-globals-to-save 'translate-shell-cache)
  (add-to-list 'desktop-globals-to-save 'translate-shell-brief-cache))

(use-package wconf
  :disabled t
  :ensure t
  :config
  (add-hook 'desktop-after-read-hook      ;so we have all buffers again
            (lambda ()
              (wconf-load)
              (wconf-switch-to-config 0)
              (add-hook 'kill-emacs-hook
                        (lambda ()
                          (wconf-store-all)
                          (wconf-save))))
            'append)

  (global-set-key (kbd "C-c w s") #'wconf-store)
  (global-set-key (kbd "C-c w S") #'wconf-store-all)
  (global-set-key (kbd "C-c w r") #'wconf-restore)
  (global-set-key (kbd "C-c w R") #'wconf-restore-all)
  (global-set-key (kbd "C-c w w") #'wconf-switch-to-config)
  (global-set-key (kbd "C-<prior>") #'wconf-use-previous)
  (global-set-key (kbd "C-<next>") #'wconf-use-next))

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

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :ensure t
  :config
  (ignoramus-setup))

(use-package dired                      ; Edit directories
  :defer t
  :config
  (use-package dired-x
    :commands dired-omit-mode
    :init (add-hook 'dired-mode-hook #'dired-omit-mode))
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

(use-package ranger
  :disabled t
  :load-path "~/wip/ranger")

(use-package bookmark
  :defer t
  :config (setq bookmark-save-flag 1))

(use-package recentf                    ; Save recently visited files
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

(use-package launch                     ; Open files in external programs
  :ensure t
  :defer t)


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
             chunyang-save-scratch
             chunyang-restore-scratch)
  :bind (([remap split-window-right] . chunyang-split-window-right)
         ([remap split-window-below] . chunyang-split-window-below)
         ;; ("M-o"                      . chunyang-other-window)
         ("C-c f w"                  . chunyang-copy-buffer-name-as-kill)
         ("C-M-!"                    . iterm-shell-command)
         ("C-x t"                    . chunyang-switch-scratch))
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

(use-package avy
  :load-path "~/wip/avy"
  :bind (("C-c SPC" . avy-goto-char)
         ("M-g f"   . avy-goto-line))
  :config
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-'") #'avy-isearch)))

(use-package ace-window
  :if (require 'avy)
  :load-path "~/wip/ace-window"
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
  :bind ("M-o" . chunyang-ace-window)
  :config
  (setq aw-ignore-current t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package easy-repeat :ensure t :defer t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :defer t
  :init (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package subword                    ; Subword/superword editing
  :defer t
  :diminish subword-mode)

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package visual-fill-column
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package zop-to-char
  :disabled t
  :ensure t
  :bind (("M-z" . zop-to-char)))

(use-package avy-zap
  :disabled t
  :if (require 'avy)
  :load-path "~/wip/avy-zap"
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package easy-kill                  ; Easy killing and marking on C-w
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

(put 'upcase-region 'disabled nil)


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
  (defun chunyang-imenu--setup-elisp ()
    ;; use-package
    (add-to-list 'imenu-generic-expression
                 `("Package" ,(rx "(use-package" (optional "-with-elapsed-timer")
                                  symbol-end (1+ (syntax whitespace)) symbol-start
                                  (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                                  symbol-end) 1)
                 )
    ;; hydra
    (add-to-list 'imenu-generic-expression
                 `("hydra" ,(rx "(defhydra"
                                symbol-end (1+ (syntax whitespace)) symbol-start
                                (group-n 1 (1+ (or (syntax word) (syntax symbol))))
                                symbol-end) 1)))
  (add-hook 'emacs-lisp-mode-hook #'chunyang-imenu--setup-elisp))

(use-package imenu-anywhere             ; Helm-based imenu across open buffers
  :ensure t
  :bind ("C-c I" . helm-imenu-anywhere))

(use-package imenu-list
  :load-path "~/wip/imenu-list")

(use-package origami :ensure t :defer t)


;;; Search
(setq isearch-allow-scroll t)

(use-package pinyin-search :ensure t :defer t)

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
  :init (global-anzu-mode)
  :config
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp)
  (setq anzu-cons-mode-line-p nil)
  :diminish anzu-mode)

(use-package which-func                 ; Current function name in header line
  :disabled t
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
  ;; Need to save my eyes
  ;; :init (add-hook 'after-init-hook #'global-color-identifiers-mode)
  )


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

(use-package yasnippet
  :disabled t
  :ensure t
  :diminish yas-minor-mode
  :config (yas-global-mode))


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
  :diminish flycheck-mode
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
    :disabled t
    :ensure t
    :config
    (eval-after-load "flycheck"
      (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))

(use-package helm-flycheck
  :ensure t
  :bind (("C-c L f" . helm-flycheck)))


;;; Text editing
(use-package iedit
  :disabled t                           ; TODO: read manual
  :ensure t
  :config (bind-key [C-return] #'iedit-rectangle-mode))


;;; Other markup languages
(use-package markdown-mode
  :ensure t
  :mode ("\\.mdpp\\'" . gfm-mode)
  :config
  (let ((stylesheet (expand-file-name
                     (locate-user-emacs-file "etc/pandoc.css"))))
    (setq markdown-command
          (mapconcat #'shell-quote-argument
                     `("pandoc" "--toc" "--section-divs"
                       "--css" ,(concat "file://" stylesheet)
                       "--standalone" "-f" "markdown" "-t" "html5")
                     " ")))
  ;; (setq markdown-command "kramdown")

  ;; No filling in GFM, because line breaks are significant.
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  ;; Use visual lines instead
  (add-hook 'gfm-mode-hook #'visual-line-mode)

  (bind-key "C-c C-s C" #'markdown-insert-gfm-code-block markdown-mode-map)
  (bind-key "C-c C-s P" #'markdown-insert-gfm-code-block markdown-mode-map))

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
  :disabled t
  :ensure t
  :diminish highlight-symbol-mode
  :defer t
  :init
  ;; Navigate occurrences of the symbol under point with M-n and M-p
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)
  ;; Highlight symbol occurrences
  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.4     ; Highlight almost immediately
        highlight-symbol-on-navigation-p t) ; Highlight immediately after
                                        ; navigation
  )

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
            (v-or-f (if found v-or-f (function-called-at-point)))
            (found (or found v-or-f)))
       (list v-or-f)))
    (if (not (and symbol (symbolp symbol)))
        (message "You didn't specify a function or variable.")
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
  ;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  ;;   (add-hook hook 'turn-on-elisp-slime-nav-mode))
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
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (defun eshell/mcd (dir)
    "make a directory and cd into it"
    (eshell/mkdir "-p" dir)
    (eshell/cd dir))
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
              ;; Setup smart shell
              ;; (require 'em-smart)
              ;; (eshell-smart-initialize)
              (bind-keys :map eshell-mode-map
                         ("TAB"     . helm-esh-pcomplete)
                         ("M-p"     . helm-eshell-history)
                         ("C-l"     . eshell-clear-buffer)
                         ("C-c C-k" . compile)
                         ("C-c C-q" . eshell-kill-process))
              (eshell/export "EDITOR=emacsclient -n")
              (eshell/export "VISUAL=emacsclient -n"))))

(use-package eshell-z
  :load-path "~/wip/eshell-z")

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

(use-package inf-ruby
  :ensure t)


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
  ;; Support SVN too, I use it
  (setq git-gutter:handled-backends '(git svn))
  ;; Live update
  (setq git-gutter:update-interval 2)
  :init
  ;; Enable globally at the beginning
  (global-git-gutter-mode))

(use-package git-messenger
  :ensure t
  :bind ("C-x v P" . git-messenger:popup-message))

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (setq magit-revert-buffers t)
  ;; Just push, no question (version 2.2.0
  (setq magit-push-always-verify nil)
  ;; Use 'C-t' to toggle the display
  (setq magit-popup-show-common-commands nil)

  ;; [[http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html][Create Github PRs from Emacs with Magit (again) · Endless Parentheses]]
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-remote)
                         "url"))
             (cdr (magit-get-remote-branch)))))
  (bind-key "v" #'endless/visit-pull-request-url magit-mode-map))

(use-package git-timemachine            ; Go back in Git time
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package gitconfig-mode             ; Edit .gitconfig files
  :ensure t
  :defer t)

(use-package gitignore-mode             ; Edit .gitignore files
  :ensure t
  :defer t)

;;; emacs vc-mode & svn
;;  - [[http://lifegoo.pluskid.org/wiki/EmacsSubversion.html][Emacs 配合 Subversion 使用]]
;;  - [[http://lifegoo.pluskid.org/wiki/EmacsVC.html][EmacsVC]]

(use-package psvn :ensure t :defer t)

;; svn issues a warning ("cannot set LC_CTYPE locale") if LANG is not set.
(setenv "LANG" "C")

(use-package ztree
  :ensure t)


;;; Tools and utilities
(use-package edit-server
  :ensure t
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

(use-package helm-open-github  :ensure t :defer t)
(use-package gh-md             :ensure t :defer t)

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
  :disabled t
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

(use-package which-key
  :load-path "~/wip/emacs-which-key"
  :config
  (setq which-key-idle-delay 1.0
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Remove my personal prefix from all bindings, since it's
          ;; only there to avoid name clashes, but doesn't add any value
          ;; at all
          ("chunyang-"     . "")))
  (which-key-mode)
  :diminish (which-key-mode . " Ⓚ"))

(use-package keyfreq
  :disabled t
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))

  (defhydra hydra-toggle (:color blue)
    "toggle"
    ("d" toggle-debug-on-error "debug")
    ("f" auto-fill-mode "fill")
    ("t" toggle-truncate-lines "truncate")
    ("w" whitespace-mode "whitespace")
    ("F" toggle-frame-fullscreen "fullscreen")
    ("m" toggle-frame-maximized "maximize")
    ("f" global-flycheck-mode "Flycheck")
    ("c" global-color-identifiers-mode "Colorful identifiers")
    ("R" writeroom-mode "Distraction-free editing")
    ("l" nlinum-mode "Line number")
    ("L" hl-line-mode "Highlight line")
    ("r" rainbow-mode "Colorize color names")
    ("q" nil "cancel"))
  (global-set-key (kbd "C-c C-v") 'hydra-toggle/body)

  (defhydra hydra-page (ctl-x-map "" :pre (widen))
    "page"
    ("]" forward-page "next")
    ("[" backward-page "prev")
    ("n" narrow-to-page "narrow" :bind nil :exit t))

  ;; (defhydra hydra-goto-line (goto-map ""
  ;;                                     :pre (linum-mode 1)
  ;;                                     :post (linum-mode -1))
  ;;   "goto-line"
  ;;   ("g" goto-line "go")
  ;;   ("m" set-mark-command "mark" :bind nil)
  ;;   ("q" nil "quit"))

  (defhydra hydra-move-text (:body-pre (use-package move-text :ensure t :defer t))
    "Move text"
    ("j" move-text-up "up")
    ("k" move-text-down "down"))

  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue))

  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra hydra-projectile (:color teal
                                     :hint nil)
    "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("s-f" projectile-find-file)
    ("ff"  projectile-find-file-dwim)
    ("fd"  projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("s-p" projectile-switch-project "switch project")
    ("p"   projectile-switch-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("`"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue)))

(use-package dash-at-point    :ensure t :defer t)
(use-package helm-dash        :ensure t :defer t)

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-global-mode)
  (use-package helm-projectile
    :if prefer-helm
    :ensure t
    :config
    (helm-projectile-on))
  :config
  (setq
   projectile-completion-system (if prefer-helm 'helm
                                  'ivy)
   projectile-mode-line '(:eval
                          (format " P[%s]"
                                  (projectile-project-name)))
   ;; Put [[https://svn.macports.org/repository/macports/users/chunyang/svn-ls-files/svn-ls-files][svn-ls-file]] into on the PATH
   projectile-svn-command "svn-ls-files")

  (defun projectile-kill-projects ()
    (interactive)
    (let ((projects
           (delq nil
                 (cl-delete-duplicates
                  (mapcar (lambda (buf)
                            (unless (string-prefix-p " " (buffer-name buf))
                              (with-current-buffer buf
                                (when (projectile-project-p)
                                  (cons (projectile-project-name) buf)))))
                          (buffer-list))
                  :test (lambda (a b) (string= (car a) (car b)))))))
      (mapc (lambda (elt)
              (with-current-buffer (cdr elt)
                (projectile-kill-buffers))) projects)
      (message "")))

  (bind-keys :map projectile-command-map
             ("K" . projectile-kill-projects)
             ;; ("s" . helm-projectile-ag)
             )
  ;; Change projectile name by using [[info:elisp#Directory%20Local%20Variables][info:elisp#Directory Local Variables]]
  (defvar my-project-name nil)

  (defun projectile-project-name--prefer-mine (orig-fun &rest args)
    (or my-project-name (apply orig-fun args)))

  (advice-add 'projectile-project-name
              :around #'projectile-project-name--prefer-mine)

  (put 'my-project-name 'safe-local-variable #'stringp))


;;; Net & Web & Email
(use-package rcirc
  :defer t
  :config
  (setq rcirc-default-nick "chunyang")
  (setq rcirc-log-flag t)
  (add-to-list 'rcirc-server-alist
               '("irc.freenode.net"
                 :channels ("#emacs" "#macports-gsoc")))
  (load-file  "~/.private.el")
  (add-hook 'rcirc-mode-hook #'flyspell-mode)
  (rcirc-track-minor-mode))

(use-package circe
  :disabled t
  :load-path "~/repos/circe/"
  :config (load-file "~/.private.el"))

(use-package mu4e
  :load-path "/opt/local/share/emacs/site-lisp/mu4e"
  :commands mu4e
  :config
  ;; Setup
  (setq mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent Messages"
        mu4e-trash-folder  "/Deleted Messages")

  (setq mu4e-refile-folder
        (lambda (msg)
          (cond
           ;; messages to the mu mailing list go to the /mu foldern
           ((mu4e-message-contact-field-matches
             msg :to "mu-discuss@googlegroups.com")
            "/mu")
           ;; messages to the emacs-devel or emacs-user mailing list go to the /Emacs folder
           ((or (mu4e-message-contact-field-matches
                 msg :to (rx (or "help-gnu-emacs@gnu.org" "emacs-devel@gnu.org")))
                (mu4e-message-contact-field-matches
                 msg :cc (rx (or "help-gnu-emacs@gnu.org" "emacs-devel@gnu.org"))))
            "/Emacs")
           ;; everything else goes to /archive
           ;; important to have a catch-all at the end!
           (t  "/Archive"))))

  (setq mu4e-attachment-dir (expand-file-name "~/Downloads"))

  (setq mu4e-confirm-quit nil)

  ;; Fetch
  (setq mu4e-get-mail-command "offlineimap")

  ;; Let me fetch new mail manually, read new mail when I'm ready.

  ;; Read
  (setq mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed" "Unread messages"      ?u)
          ("date:today..now"                  "Today's messages"     ?t)
          ("date:7d..now"                     "Last 7 days"          ?w))
        mu4e-maildir-shortcuts
        '(("/INBOX"               . ?i)
          ("/Sent Messages"       . ?s)
          ("/Archive"             . ?a)))

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

  ;; something about ourselves
  (setq user-mail-address "xu.chunyang@icloud.com"
        user-full-name  "Chunyang Xu"
        mu4e-compose-signature
        "Chunyang Xu\n")

  ;; Send
  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil))
        smtpmail-auth-credentials  (expand-file-name "~/.authinfo")
        smtpmail-default-smtp-server "smtp.mail.me.com"
        smtpmail-smtp-server "smtp.mail.me.com"
        smtpmail-smtp-service 587
        starttls-extra-arguments nil
        starttls-gnutls-program (executable-find "gnutls-cli")
        starttls-use-gnutls t)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; org-mode support
  (require 'org-mu4e)
  (use-package mu4e-maildirs-extension  ; Show maildirs summary in mu4e-main-view
    :ensure t
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
  :load-path "~/wip/youdao-dictionary"
  :bind ("C-c y" . youdao-dictionary-search)
  :config (setq url-automatic-caching t))

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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELED(c@)")))

  (setq org-directory "~/Dropbox/Notes")
  (setq org-agenda-files (list org-directory))

  (bind-key "<f12>" #'org-agenda-list)
  (bind-key "<f11>" #'org-clock-goto)

  (setq org-adapt-indentation nil)

  (setq org-default-notes-file "~/Dropbox/Notes/notes.org")

  (setq org-capture-templates
        '(("t" "todo"
           entry (file (expand-file-name "refile.org" org-directory))
           "* TODO %?\n%i\n%a")
          ("n" "note"
           entry (file (expand-file-name "refile.org" org-directory))
           "* %?\n%i\n%a")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODO's" ((agenda "") (alltodo "")))
          ("e" "Emacs-related tasks" tags-todo "+emacs")
          ("g" "GSoC-related tasks" tags-todo "+gsoc")))

  (setq org-log-done 'time)

  ;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 3)
                                   (org-agenda-files :maxlevel . 3))))

  ;; Clock work time
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (setq org-clock-persist-query-resume nil)

  (use-package org-mac-link
    :if (eq system-type 'darwin)
    :ensure t
    :commands (;; org-mac-firefox-insert-frontmost-url
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

(use-package org-plus-contrib           ; Various org-mode extensions
  :disabled                             ; only available from org elpa
  ;; Just install, don't require that feature
  :ensure t :defer t)

(use-package toc-org
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-enable))

(use-package orglink
  :ensure t
  :diminish orglink-mode
  :init (global-orglink-mode))

(use-package org-bullets
  :disabled t
  :ensure t
  :config (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package calfw
  :ensure t :defer t
  :init (use-package calfw-org :commands cfw:open-org-calendar))

(bind-key "C-h h" #'describe-personal-keybindings)
(bind-key "C-h C-k" #'find-function-on-key)

(use-package paradox
  :ensure t
  :config
  ;; Don't ask for a token, please, and don't bug me about asynchronous updates
  (setq paradox-github-token t
        paradox-execute-asynchronously nil)
  (defun chunyang-visit-package-homepage (pkg)
    (interactive (list
                  (intern
                   (completing-read "Package: "
                                    (mapcar
                                     (lambda (pkg) (symbol-name (car pkg)))
                                     ;; FIXME: `package-alist' only for
                                     ;; installed package while I also need
                                     ;; other packages as well.
                                     package-alist)
                                    nil t))))
    (let ((url (paradox--package-homepage pkg)))
      (if url
          (browse-url url)
        (message "Package %s has no homepage"
                 (propertize (symbol-name pkg)
                             'face 'font-lock-keyword-face)))))

  (defun lunaryorn-browse-feature-url (feature)
    "Browse the URL of the given FEATURE.

Interactively, use the symbol at point, or prompt, if there is
none."
    (interactive
     (let ((symbol (or (symbol-at-point)
                       (completing-read "Feature: " features nil
                                        'require-match))))
       (list symbol)))
    (require 'find-func)
    (require 'lisp-mnt)
    (let* ((library (if (symbolp feature) (symbol-name feature) feature))
           (library-file (find-library-name library)))
      (when library-file
        (with-temp-buffer
          (insert-file-contents library-file)
          (if-let ((url (or (lm-header "Homepage") (lm-header "URL"))))
              (browse-url url)
            (user-error "Library %s has no URL header" library)))))))


;;; My own ELPA package management utilities

;; TODO: Implement `:config'
;; TODO: Implement `bind'
;; TODO: Implement `:if'
(defmacro use-pkg (name &rest args)
  (declare (indent 1))
  (unless (member :disabled args)
    (unless (package-installed-p name)
      (package-install name))))

(use-pkg string-edit)

;;; init.el ends here
