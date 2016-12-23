;;; init.el --- Chunyang Xu's Emacs Configuration  -*- lexical-binding: t; -*-

;;; Debugging
(setq message-log-max 10000)


;;; Start up

;; Don't load outdated byte code
(setq load-prefer-newer t)

;; OK, redefinition is fine for me
(setq ad-redefinition-action 'accept)

(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.emacs-china.org/gnu/")
        ("melpa"        . "https://elpa.emacs-china.org/melpa/")
        ("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("marmalade"    . "https://elpa.emacs-china.org/marmalade/")
        ("org"          . "https://elpa.emacs-china.org/org/")
        ("user42"       . "https://elpa.emacs-china.org/user42/")
        ("SC"           . "https://elpa.emacs-china.org/sunrise-commander/")))

(setq package-archives
      '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
        ("melpa" . "http://elpa.emacs-china.org/melpa/")
        ;; Using org from Git
        ;; ("org"   . "https://elpa.emacs-china.org/org/")
        ;; `gtk-look'
        ("user42" . "http://elpa.emacs-china.org/user42/")
        ;; ("SC"     . "https://elpa.emacs-china.org/sunrise-commander/")
        ))

;; Different Emacs versions can't share the same elpa folder
(setq package-user-dir (locate-user-emacs-file
                        (format "elpa-%d-%d"
                                emacs-major-version
                                emacs-minor-version)))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :if (version<= "25" emacs-version)
  :config
  (define-advice use-package-ensure-elpa (:before (&rest r) fill-selected)
    "Make sure :ensure fill `package-selected-packages'."
    (add-to-list 'package-selected-packages (car r))))

(use-package use-package :ensure t)

;; Define `config' to group package configuration (an alternative to
;; `use-package')
(defmacro config (pkg &rest args)
  "Group PKG's configuration in one place."
  ;; TODO: :if
  ;; TODO: :ensure
  ;; TODO: :bind
  ;; TODO: Handle error (by wrapping `condition-case'?)
  (declare (indent defun))
  (if (memq :disabled args)
      (let ((t-or-nil (plist-get args :disabled)))
        (setq args (delq :disabled args))
        (setq args (delq t-or-nil args))
        (unless t-or-nil
          `(progn ,@args)))
    `(progn ,@args)))

;; My private packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; Emacs Compatibility

(use-package chunyang-emacs-compatibility)


;;; Require helper libraries

(require 'subr-x)

;; Make sure `seq.el' is installed for Emacs 24
(unless (version< emacs-version "25")
  (use-package seq :ensure t :defer t))

(require 'rx)
(use-package dash
  :defer t
  :ensure t
  :config (dash-enable-font-lock))

;; http://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html
(defmacro measure-time (&rest body)     ; oops, try (info "(elisp) Profiling")
                                        ; instead
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))


;;; Initialization

;; And disable the site default settings (TODO: but what is it exactly?)
(setq inhibit-default-init t)

;; NO MORE AD
;; Emacs 处理这个变量的方式挺有意思的，如果你的配置要还给其他人用，最好不去动这个变量
(setq inhibit-startup-echo-area-message "xcy")

;; Load personal information
(load "~/.private.el" :no-error)

;; Make ~/.emacs.d/ clean by putting package files into var/ and etc/
(use-package no-littering
  ;; Prefer local version if the directory exists
  :load-path "~/src/no-littering"
  :ensure t)


;;; OS X support

(defconst *is-mac* (eq system-type 'darwin))

(use-package ns-win                     ; OS X window support
  :defer t
  :if *is-mac*
  :config
  (setq ns-pop-up-frames nil ; Don't pop up new frames from the workspace
        mac-command-modifier 'meta
        mac-option-modifier 'control))

(use-package exec-path-from-shell
  :if (eq window-system 'ns)
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; For passing aliases
;; (setq shell-command-switch "-ic")

(use-package chunyang-osx
  :if *is-mac*
  :commands (restart-emacs omnifocus-new-entry)
  :bind (
         ;; ("<wheel-left>"         . chunyang-next-buffer)
         ;; ("<double-wheel-left>"  . chunyang-next-buffer)
         ;; ("<triple-wheel-left>"  . chunyang-next-buffer)
         ;; ("<wheel-right>"        . chunyang-previous-buffer)
         ;; ("<double-wheel-right>" . chunyang-previous-buffer)
         ;; ("<triple-wheel-right>" . chunyang-next-buffer)
         ))


;;; GNU/Linux

(defconst *is-gnu-linux* (eq system-type 'gnu/linux))

(when *is-gnu-linux*
  (defun insert-x11-primary-selection ()
    (interactive)
    (insert (gui-get-primary-selection)))
  (bind-key "<S-insert>" #'insert-x11-primary-selection))

(use-package chunyang-linux
  :if *is-gnu-linux*
  :commands chunyang-linux-gnome-terminal-cd)

(use-package grab-x-link
  :if *is-gnu-linux*
  :load-path "/home/xcy/src/grab-x-link")


;;; User Interface

;; It's possible that Emacs is not built with Tool Bar and Scroll Bar support,
;; so we need to test firstly
(when (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(when (bound-and-true-p scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off `menu-bar-mode' unless on MacOS
(unless *is-mac* (menu-bar-mode -1))

(setq inhibit-startup-screen t)

(setq ring-bell-function #'ignore)      ; Don't ring bell on C-g


;;; Yes Or No

(defvar Orig-yes-or-no-p (symbol-function 'yes-or-no-p))

(defun yes-or-no-p@or-just-y-or-n-p (orig-fun prompt)
  (funcall
   (if (or
        ;; refresh in Help buffer
        (and (string= (buffer-name) "*Help*")
             (eq this-command 'revert-buffer))
        (eq this-command 'projectile-kill-buffers)
        (eq this-command 'git-gutter:stage-hunk)
        (eq this-command 'git-gutter:revert-hunk)
        (eq this-command 'org-ctrl-c-ctrl-c)
        (eq this-command 'org-open-at-point)
        (eq this-command 'kill-this-buffer))
       #'y-or-n-p Orig-yes-or-no-p)
   prompt))
(advice-add 'yes-or-no-p :around #'yes-or-no-p@or-just-y-or-n-p)

;; Answer yes automatically in some commands
(defun yes-or-no-bypass (orig-fun &rest args)
  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t)))
    (apply orig-fun args)))

(defcustom yes-or-no-bypass-functions '(projectile-kill-buffers)
  "Functions which bypass `yes-or-no-p'."
  :type '(repeat (choice function))
  :set (lambda (var val)
         (set var val)
         (dolist (sym yes-or-no-bypass-functions)
           (advice-add sym :around 'yes-or-no-bypass))))

;; This is not safe
(fset 'yes-or-no-p #'y-or-n-p)


;;; Font

;; TODO: I have trouble setting English and Chinese fonts
(cl-case window-system
  ('ns (set-face-attribute 'default nil :font "Source Code Pro-13"))
  ('x (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font)
                          charset
                          (font-spec :family "Noto Sans Mono CJK SC" :size 14)))))


;; Theme

;; I change theme periodically so I don't want hard-code it here,
;; instead, I use the Custom via 'M-x customize-themes'.

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init (setq spacemacs-theme-comment-bg nil
              spacemacs-theme-org-height nil))

(use-package tomorrow-theme
  :ensure color-theme-sanityinc-tomorrow
  :defer t)

;; Mode line
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
                (helm-alive-p
                 (:eval (when (eq (current-buffer) helm-current-buffer)
                          (propertize " Helm-current " 'face 'bold))))
                ;; Other modes
                " " mode-line-modes mode-line-misc-info mode-line-end-spaces))

(column-number-mode)
(size-indication-mode)

(use-package spaceline
  :disabled t
  :load-path "~/wip/spaceline"
  ;; :defer t
  :init
  (require 'spaceline-config)
  ;; (spaceline-spacemacs-theme)
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  (spaceline-info-mode)
  :config
  (setq powerline-default-separator 'wave)
  (setq spaceline-workspace-numbers-unicode t)
  (setq anzu-cons-mode-line-p nil))

;; Echo Area
(setq echo-keystrokes 0.6)

;; M-x display-time-world
(with-eval-after-load 'time
  (add-to-list 'display-time-world-list '("Asia/Shanghai" "上海")))


;; 设置引号的样式，默认 curve 样式会把「`like this'」显示成「‘like
;; this’」，虽然美观，但给我造成了问题
;;
;; @Emacs-25 @unstable @new-feature
(setq text-quoting-style 'grave)

;;; Emacs session persistence
(use-package desktop                    ; frame/window/buffer and global vars
  :disabled t
  :if (display-graphic-p)
  :init
  (setq desktop-load-locked-desktop nil)
  (desktop-save-mode)
  :config
  (setq desktop-restore-frames nil)
  ;; Save the content of *scratch* across sessions
  (add-to-list 'desktop-globals-to-save 'initial-scratch-message)
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (get-buffer "*scratch*")
                (with-current-buffer "*scratch*"
                  (widen)
                  ;; FIXME: Emacs deals with `initial-scratch-message' with
                  ;; `substitute-command-keys', which does a lot unwanted
                  ;; conversions.
                  (setq initial-scratch-message
                        (buffer-string)))))))

;; Maximize Emacs frame on start-up
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


(use-package savehist                   ; Minibuffer history
  ;; :disabled t
  :defer t
  :init
  (savehist-mode)
  (setq history-length 1000
        history-delete-duplicates t
        savehist-additional-variables '(extended-command-history)))

(use-package recentf                    ; Recent files
  :defer t
  :config
  (setq recentf-max-saved-items 512
        recentf-exclude (list "/\\.git/.*\\'"      ; Git contents
                              "/\\.emacs\\.d/elpa" ; ELPA
                              "/etc/.*\\'"         ; Package configuration
                              "/var/.*\\'"         ; Package data
                              ".*\\.gz\\'"
                              "TAGS"
                              ".*-autoloads\\.el\\'"))

  ;; Reopen Last Closed File
  (defun reopen-last-closed-file ()
    (interactive)
    (find-file (car recentf-list)))

  ;; (define-key (current-global-map) "\M-z" #'reopen-last-closed-file)
  )

(use-package bookmark
  :defer t
  :config
  ;; Save immediately when make or delete a bookmark
  (setq bookmark-save-flag 1))

(use-package saveplace                  ; Save point position in files
  :if (version< "25" emacs-version)
  :defer t
  :init (add-hook #'after-init-hook #'save-place-mode))


;;; Minibuffer with helm

(defcustom chunyang-completion-system 'helm
  "My preferred completion system."
  :type '(radio (const default)
                (const ido)
                (const ivy)
                (const helm)))

(use-package chunyang-helm
  :if (eq chunyang-completion-system 'helm))

(use-package ido
  :if (eq chunyang-completion-system 'ido)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t)
  (ido-mode))

(use-package ivy
  :if (eq chunyang-completion-system 'ivy)
  ;; FIXME: Why :if doesn't take over :ensure
  :disabled (eq chunyang-completion-system 'ivy)
  :ensure swiper
  :bind (("C-z"    . ivy-resume)
         ("C-s"    . swiper)
         ("M-l"    . ivy-switch-buffer)
         ("C-x f"  . ivy-recentf))
  :config
  (ivy-mode)
  (bind-key "C-o" 'imenu)
  (use-package counsel
    :ensure t
    :bind ("M-x" . counsel-M-x)))


;;; Buffers, Windows and Frames

(use-package uniquify            ; Make buffer names unique (turn on by default)
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :diminish auto-revert-mode
  :init (global-auto-revert-mode))

(use-package chunyang-simple
  :demand t
  :bind (("C-x 3" . chunyang-split-window-right)
         ("C-x 2" . chunyang-split-window-below)
         ("C-h t" . chunyang-switch-scratch)
         :map lisp-interaction-mode-map
         ("C-c C-l" . scratch-clear)))

(use-package chunyang-misc)

(use-package chunyang-buffers           ; Personal buffer tools
  :config (add-hook 'kill-buffer-query-functions
                    #'lunaryorn-do-not-kill-important-buffers))

(bind-key "O" #'delete-other-windows  special-mode-map)
(bind-key "Q" #'kill-this-buffer      special-mode-map)
(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-x K" #'kill-buffer)

(use-package ace-window
  :ensure t
  :defer t
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

(use-package winner
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config (winner-mode))

(use-package eyebrowse
  :disabled t
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
  :preface
  (defun chunyang-frame-left-half ()
    (interactive)
    (set-frame-width (selected-frame) (/ 177 2))
    (set-frame-position (selected-frame) 0 23))
  (defun chunyang-frame-right-half ()
    (interactive)
    (set-frame-width (selected-frame) (/ 177 2))
    (set-frame-position (selected-frame) 720 23))
  (defun chunyang-frame-center ()
    (interactive)
    (let ((frame (selected-frame)))
      (set-frame-width frame 93)
      (set-frame-height frame 36)
      (set-frame-position frame 337 104)))
  :bind (("C-c t F" . toggle-frame-fullscreen)
         ("C-c t m" . toggle-frame-maximized)
         ("C-c w f" . toggle-frame-maximized)
         ("C-c w l" . chunyang-frame-left-half)
         ("C-c w r" . chunyang-frame-right-half)
         ("C-c w c" . chunyang-frame-center))
  :config
  ;; (add-to-list 'initial-frame-alist '(maximized . fullscreen))
  (unbind-key "C-x C-z"))


;;; File handle
;; Keep backup and auto save files out of the way
;; NOTE: Commenting this out because `no-littering.el' is doing this for me
;; (setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
;;       auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Delete files to trash
(setq delete-by-moving-to-trash t)

(use-package files
  :bind (("C-c f u" . revert-buffer)
         ("C-c f n" . normal-mode))
  :config
  ;; Prefer GNU variants
  (when (eq 'darwin system-type)
    (setq insert-directory-program  "gls"
          find-program              "gfind"
          grep-program              "ggrep")))

(use-package dired                      ; Directory Editor
  :defer t
  :config
  ;; --ignore=.git*
  (setq dired-listing-switches "-alh --no-group --ignore=.git*")
  (use-package dired-x
    :commands dired-omit-mode
    :init (add-hook 'dired-mode-hook #'dired-omit-mode)))

(use-package direx                      ; Alternative to Dired
  :ensure t
  :defer t)

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

;; I have used 'M-l' to run `helm-mini' for a long time
(define-key global-map "\M-L" #'downcase-dwim)

;; Configure a reasonable fill column, indicate it in the buffer and enable
;; automatic filling
(setq-default fill-column 80)

;; I prefer indent long-line code myself
;; (setq comment-auto-fill-only-comments t)

;; (add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)

;; (diminish 'auto-fill-function)          ; Not `auto-fill-mode' as usual

;; (global-visual-line-mode)

(use-package visual-fill-column
  ;; TODO: use-package: 自定义关键词
  ;; :description "定制 Emacs 自带 visual-line-mode 的宽度"
  ;; :url "foobar"
  :ensure t
  :defer t
  :init
  (setq visual-fill-column-width fill-column)
  ;; (setq visual-fill-column-fringes-outside-margins nil)
  (add-hook 'visual-line-mode-hook
            (defun visual-fill-column-toggle ()
              (visual-fill-column-mode (or visual-line-mode -1)))))

;; NOTE: `visual-line-mode' 似乎假定了字与字之间使用空格隔开的！？所以处理中文应该有问题吧！
;;       好像有没问题！我不是 100% 确定到底有没有问题，至少目前没发现问题。
(with-eval-after-load 'org
  ;; (add-hook 'org-mode-hook 'visual-line-mode)
  )

(use-package whitespace-cleanup-mode    ; Cleanup whitespace in buffers
  :disabled t
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  ;; :diminish whitespace-cleanup-mode
  )

(use-package subword                    ; Subword/superword editing
  :defer t
  ;; :diminish subword-mode
  )

(use-package adaptive-wrap              ; Choose wrap prefix automatically
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package avy
  :disabled t
  :ensure t
  :bind (("M-g c" . avy-goto-char)
         ("M-g l" . avy-goto-line))
  :config
  (with-eval-after-load "isearch"
    (define-key isearch-mode-map (kbd "C-'") #'avy-isearch)))

(use-package pin :disabled t)

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
  :disabled t
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package drag-stuff
  :disabled t
  :defer t
  :ensure t)

(use-package align                      ; Align text in buffers
  :bind (("C-c A a" . align)
         ("C-c A c" . align-current)
         ("C-c A r" . align-regexp)))

(use-package multiple-cursors           ; Edit text with multiple cursors
  :disabled t
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

(put 'timer-list 'disabled nil)

(use-package help-mode
  :preface
  (defun view-help-buffer ()
    "View the `*Help*' buffer."
    (interactive)
    (pop-to-buffer (help-buffer)))
  (defun chunyang-clear-messages-buffer ()
    "Delete the contents of the *Messages* buffer."
    (interactive)
    (with-current-buffer "*Messages*"
      (let ((inhibit-read-only t))
        (erase-buffer))))
  (defun help-info-lookup-symbol ()
    (interactive)
    (when-let ((symbol (cadr help-xref-stack-item)))
      (info-lookup-symbol symbol)))
  (defun chunyang-describe-symbol-at-point ()
    "Like `describe-symbol' but doesn't query always."
    (interactive)
    (require 'help-mode)
    (let* ((is-symbol-p
            (lambda (vv)
              (cl-some (lambda (x) (funcall (nth 1 x) vv))
                       describe-symbol-backends)))
           (sym
            (or (let ((it (intern (current-word))))
                  (when (funcall is-symbol-p it)
                    it))
                (completing-read
                 "Describe symbol: "
                 obarray
                 is-symbol-p
                 t))))
      (describe-symbol sym)))
  :bind (("C-h ." . chunyang-describe-symbol-at-point)
         ("C-h h" . view-help-buffer)
         :map help-mode-map
         ("b" . help-go-back)
         ("f" . help-go-forward)
         ("i" . help-info-lookup-symbol)))

(use-package info-look
  :defer t
  :config
  (info-lookup-add-help
   :mode 'emacs-lisp-mode
   :regexp "[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(emacs)Command Index"             nil "['`‘]\\(M-x[ \t\n]+\\)?" "['’]") ;
               ("(emacs)Variable Index"            nil "['`‘]" "['’]")
               ("(elisp)Index"                     nil "^ -+ .*: " "\\( \\|$\\)")
               ;; cl-lib
               ("(cl) Function Index"              nil "^ -+ .*: " "\\( \\|$\\)")
               ("(cl) Variable Index"              nil "^ -+ .*: " "\\( \\|$\\)")
               ;; Org
               ("(org) Variable Index"             nil "['`‘]" "['’]")
               ("(org) Command and Function Index" nil "['`‘(]" "['’)]")
               ;; Magit
               ("(magit) Command Index"            nil "(['`‘]" "['’])")
               ("(magit) Variable Index"           nil "^ -+ .*: " "\\( \\|$\\)"))))

(use-package info
  :defer t
  :config
  ;; IDEA Track info browse history
  ;; (defvar chunyang-Info-visited-nodes nil)
  ;; (defun chunyang-Info-track-history ()
  ;;   (list (file-name-nondirectory Info-current-file)
  ;;         Info-current-node)
  ;;   (message "todo..."))
  ;; (add-hook 'Info-selection-hook 'chunyang-Info-track-history)

  ;; Get HTML link
  ;; (emacs) Echo Area
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Echo-Area.html
  ;;
  ;; (elisp) The Echo Area
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html
  ;; IDEA Support not only Emacs Info manuals
  (defun Info-browse-and-copy-html-url ()
    (interactive)
    (cl-assert (eq major-mode 'Info-mode))
    (let ((file (file-name-nondirectory Info-current-file))
          (node Info-current-node))
      (let ((url (format "https://www.gnu.org/software/emacs/manual/html_node/%s/%s.html"
                         file
                         (replace-regexp-in-string " " "-" node))))
        (browse-url url)
        (kill-new url)
        (message "Copied: %s" url))))
  (bind-key "C" 'Info-browse-and-copy-html-url Info-mode-map))

(use-package cus-edit
  :preface
  (defun chunyang/custom-mode-describe-symbol-at-point ()
    (interactive)
    (require 'info-look)
    (let ((symbol (intern (downcase (info-lookup-guess-custom-symbol)))))
      (describe-symbol symbol)))
  :bind (:map custom-mode-map
              ("C-h ." . chunyang/custom-mode-describe-symbol-at-point)))

(use-package command-log-mode           ; BUG: Create a new empty buffer and
                                        ; insert some text, should blame
                                        ; function added to post-self-insert-hook
  :disabled t
  :ensure t)


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

;; (bind-key* "C-M-p" #'scroll-up-line)
;; (bind-key* "C-M-n" #'scroll-down-line)
;; Use `C-M-l' instead of twice `C-l' for a better view

(use-package page-break-lines           ; Turn page breaks into lines
  :ensure t
  :diminish page-break-lines-mode
  :defer t
  :preface
  (defun add-hook* (hooks funs)
    (dolist (hook (if (listp hooks) hooks (list hooks)))
      (dolist (fun (if (listp funs) funs (list funs)))
        (add-hook hook fun))))
  :init (add-hook* '(prog-mode-hook help-mode-hook) #'page-break-lines-mode))

(use-package outline                    ; Navigate outlines in buffers
  :disabled t
  :diminish outline-minor-mode
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'outline-minor-mode)))

(use-package imenu
  :defer t
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

(use-package re-builder
  :defer t
  :config
  ;; Escape 的工作就交给 Emacs 了
  (setq reb-re-syntax 'string))

(use-package grep
  :defer t
  :config
  (dolist (file '("TAGS" "GPATH" "GRTAGS" "GTAGS"))
    (add-to-list 'grep-find-ignored-files file))
  (add-to-list 'grep-find-ignored-directories "auto")
  (add-to-list 'grep-find-ignored-directories "elpa")
  (use-package wgrep :ensure t :defer t))

(use-package anzu                       ; Position/matches count for isearch
  :disabled t
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :config
  :config
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" 'anzu-query-replace)
  (bind-key "C-M-%" 'anzu-query-replace-regexp))

;; TODO: Consider adding `:todo' keyword to `use-package', just for some fun,
;;       it's ok that it's not necessary and useful
;; TODO: Errors on {her,his} -> chunyang
(use-package plur
  :ensure t
  :load-path "~/src/plur"
  :bind ("C-c M-%" . plur-query-replace)
  :config (define-key isearch-mode-map "\M-{" #'plur-isearch-query-replace))

(use-package region-state
  :ensure t
  :commands region-state-mode
  :init (region-state-mode))

(use-package swap-regions
  :ensure t
  :bind ("C-c C-t" . swap-regions)
  :commands swap-regions-mode
  :init (swap-regions-mode))

(use-package clear-text
  :load-path "~/src/clear-text.el"
  :commands (clear-text-mode global-clear-text-mode))

(use-package pinyin-search
  :ensure t
  :defer t)


;;; Highlight
(use-package whitespace                 ; Highlight bad whitespace (tab)
  :bind ("C-c t w" . whitespace-mode)
  ;; :init (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-style
        '(face
          trailing
          ;; empty lines at beginning and/or end of buffer
          ;; empty
          ;; line is longer `fill-column'
          lines-tail
          ;; If `indent-tabs-mode' on, visualize spaces at the beginning of the
          ;; line, otherwise, visualize tabs.
          indentation
          ;; Visualize TAB
          tab-mark))
  (setq whitespace-line-column nil)
  :diminish " Whitespace")

(use-package hl-line
  :bind ("C-c t L" . hl-line-mode))

(use-package paren                      ; Highlight paired delimiters
  :init (show-paren-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'hl-todo-mode)
  :config
  (add-to-list 'hl-todo-keyword-faces '("IDEA" . "#d0bf8f")))

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
  :disabled t
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
  :defer t
  :init (add-hook 'css-mode-hook #'rainbow-mode))

(use-package hl-issue-id
  :disabled t
  :load-path "~/src/emacs-hl-issue-id"
  :config (global-hl-issue-id-mode))


;;; Skeletons, completion and expansion

(use-package abbrev                     ; For fixing typo only for now
  :disabled t
  :defer t
  :init
  (setq only-global-abbrevs t)
  ;; Enable this mode globally
  (setq-default abbrev-mode t)
  :diminish abbrev-mode)

(use-package hippie-exp                 ; Powerful expansion and completion
  :bind ([remap dabbrev-expand]  . hippie-expand)
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
  ;; Use Company for completion C-M-i
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; M-h/c-h/F1 to display doc in help buffer, C-w to show location
  (define-key company-active-map "\M-h" #'company-show-doc-buffer)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t))

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
  :init
  ;; (add-hook 'text-mode-hook #'flyspell-mode)
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (use-package ispell
    :defer t
    :init
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra")))
  :bind ("C-c t s" . flyspell-mode)
  :config
  (unbind-key "C-."   flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-;"   flyspell-mode-map)
  (use-package flyspell-popup
    :ensure t
    :config
    (define-key flyspell-mode-map [?\C-.] #'flyspell-popup-correct)
    (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)))

(use-package checkdoc
  :disabled t                           ; Not working anyway
  :config (setq checkdoc-arguments-in-order-flag nil
                checkdoc-force-docstrings-flag nil))

(use-package flycheck
  :ensure t
  :bind (("C-c t f" . global-flycheck-mode))
  :config

  (setq flycheck-emacs-lisp-load-path 'inherit)

  (defun chunyang-flycheck-toggle-checkdoc ()
    (interactive)
    (setq-local flycheck-disabled-checkers
                (if (memq 'emacs-lisp-checkdoc flycheck-disabled-checkers)
                    (delq 'emacs-lisp-checkdoc flycheck-disabled-checkers)
                  (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers)))
    (when flycheck-mode
      (flycheck-mode 'toggle)
      (flycheck-mode 'toggle)))

  (use-package flycheck-pos-tip           ; Show Flycheck messages in popups
    :disabled t
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

;; TODO: Fontify markdown link like org, see `org-descriptive-links',
;; `orglink', and (elisp) Font Lock Mode.
(use-package markdown-mode
  :ensure t
  :defer t
  ;; :mode ("README\\.md\\'" . gfm-mode)
  ;; :config
  ;; (setq markdown-command "pandoc -s -f markdown -t html")
  )

(use-package yaml-mode :ensure t :defer t)


;;; Programming utilities

;; `glasses-mode' -- 把 areYouReady 显示成 are_You_Ready
;; `subword-mode' -- 把 StudlyCapsIdentifiers 当作一个 word
;; `superword-mode' -- 把 this_is_a_symbol 当作一个 word
;; `hs-minor-mode' -- Hideshow / 折叠、隐藏

(cl-defun chunyang-project-root (&optional (dir default-directory))
  "Return project root in DIR, if no project is found, return DIR."
  (or (cdr (project-current nil dir))
      dir))

(use-package compile
  :bind (("C-x c" . compile))
  :preface

  (defun chunyang-ansi-color-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))

  (defvar chunyang-compilation-root nil)
  (defun chunyang-compilation-setup ()
    (setq chunyang-compilation-root (chunyang-project-root)))
  (defun chunyang-compilation-save-buffers-predicate ()
    (file-in-directory-p (buffer-file-name) chunyang-compilation-root))

  :config
  ;; Colorize ansi escape color code
  (add-hook 'compilation-filter-hook 'chunyang-ansi-color-compilation-buffer)

  ;; Only ask for saving files under current project
  (setq compilation-process-setup-function 'chunyang-compilation-setup)
  (setq compilation-save-buffers-predicate 'chunyang-compilation-save-buffers-predicate)

  (setq compilation-always-kill t
        compilation-scroll-output 'first-error))

(use-package quickrun
  :ensure t :defer t)

(use-package prog-mode
  :bind ("C-c t p" . prettify-symbols-mode)
  ;; TODO: I have some font issue, so disalbe it for now
  ;; :init (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
  ;; :init (global-prettify-symbols-mode)
  )


;;; Generic Lisp
(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :init (add-hook 'lisp-mode-hook 'paredit-mode)
  :config
  (unbind-key "M-r" paredit-mode-map) (bind-key "M-R" #'paredit-raise-sexp  paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map) (bind-key "M-S" #'paredit-splice-sexp paredit-mode-map)
  (unbind-key "C-j" paredit-mode-map)
  (unbind-key "M-q" paredit-mode-map)
  (unbind-key "M-?" paredit-mode-map)
  (unbind-key "M-;" paredit-mode-map)

  (use-package paredit-menu
    :ensure t
    :commands menubar-paredit))

(use-package adjust-parens              ; TODO: Try it!!!
  :disabled t
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook #'adjust-parens-mode))


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
  (use-package ipretty
    :disabled t                         ; Turn off for some time
    :ensure t
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'ipretty-mode))

  ;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  ;;   (add-hook hook 'turn-on-elisp-slime-nav-mode))
  (use-package aggressive-indent
    :ensure t
    :defer t
    :diminish aggressive-indent-mode
    :init
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

  (when (version< emacs-version "25")
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))

(use-package el-search
  :if (version< "25" emacs-version)
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'elisp-mode
    (define-key emacs-lisp-mode-map [(control ?S)] #'el-search-pattern)
    (define-key emacs-lisp-mode-map [(control ?%)] #'el-search-query-replace)
    (define-key lisp-interaction-mode-map [(control ?S)] #'el-search-pattern)
    (define-key lisp-interaction-mode-map [(control ?%)] #'el-search-query-replace))

  (with-eval-after-load 'isearch
    (define-key isearch-mode-map [(control ?S)] #'el-search-search-from-isearch)
    (define-key isearch-mode-map [(control ?%)] #'el-search-replace-from-isearch))
  (with-eval-after-load 'el-search
    (define-key el-search-read-expression-map [(control ?S)] #'exit-minibuffer)))

(use-package chunyang-elisp
  :config
  (bind-key "C-M-;" #'comment-or-uncomment-sexp emacs-lisp-mode-map)
  (bind-keys :map emacs-lisp-mode-map
             ("C-j" . chunyang-eval-print-last-sexp)
             ("C-," . chunyang-macroexpand-print-last-sexp)
             :map lisp-interaction-mode-map
             ("C-j" . chunyang-eval-print-last-sexp)
             ("C-," . chunyang-macroexpand-print-last-sexp))
  (with-eval-after-load 'aggressive-indent
    (push 'chunyang-eval-print-last-sexp
          aggressive-indent-protected-commands)
    (push 'chunyang-macroexpand-print-last-sexp
          aggressive-indent-protected-commands)))

(use-package chunyang-package
  :commands chunyang-package-homepage)

(use-package ielm
  :defer t
  :config
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

(use-package macrostep
  :ensure t
  :bind ("C-c e" . macrostep-expand))

(use-package pcache              :ensure t :defer t)
(use-package persistent-soft     :ensure t :defer t)
(use-package log4e               :ensure t :defer t)
(use-package alert               :ensure t :defer t)

(use-package bug-hunter                 ; This is good
  :disabled t
  :ensure t :defer t)

(use-package debbugs                    ; Interface to GNU Bugs
  :ensure t
  :defer t
  :preface
  ;; TODO: Fontify #1234 (make it clickable) in certain modes
  (defun chunyang-open-debbug-gnu-bug-by-id (id)
    (interactive (list
                  (or (number-at-point)
                      (and (looking-at "#\\([0-9]*\\)")
                           (string-to-number (match-string 1)))
                      (read-number "Bug Id: "))))
    (let ((url
           (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                   id)))
      (message "Opening %s ..." (propertize url 'face 'link))
      (browse-url url))))

(use-package hydra :ensure t :defer t)

(use-package debug
  :defer t
  :config
  (defhydra hydra-debugger-menu ()
    "Debug"
    ("c" debugger-continue "Continue")
    ("e" debugger-eval-expression "Eval")
    ("v" debugger-toggle-locals "Display local Variable"))

  (define-key debugger-mode-map "." 'hydra-debugger-menu/body))

;; TODO Add (info "(elisp) Edebug")

(use-package nameless
  :ensure t
  :defer t)

(use-package package-lint               ; Check Compatibility
  :ensure t
  :defer t)


;;; Help

(bind-key "C-h C-k" #'find-function-on-key)


;;; Version Control
(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :init (global-git-commit-mode)
  :config
  ;; Hide popup by default, type `C-t' to display if need
  (setq magit-popup-show-common-commands nil)
  ;; Save files before executing git command for me
  (setq magit-save-repository-buffers 'dontask)
  ;; Show word-granularity differences within diff hunks
  ;; (setq magit-diff-refine-hunk 'all)

  ;; My answer to https://emacs.stackexchange.com/questions/28502/magit-show-ignored-files
  ;; Command to list ignored files:
  ;; $ git ls-files --others --ignored --exclude-standard --directory
  (defun magit-ignored-files ()
    (magit-git-items "ls-files" "--others" "--ignored" "--exclude-standard" "-z" "--directory"))

  (defun magit-insert-ignored-files ()
    (-when-let (files (magit-ignored-files))
      (magit-insert-section (ignored)
        (magit-insert-heading "Ignored files:")
        (magit-insert-un/tracked-files-1 files nil)
        (insert ?\n))))

  :preface
  (defun Info-goto-node--gitman-for-magit (orig-fun &rest r)
    "Handle gitman info link for Magit info.

I don't want to install git info page, because it doesn't worth.
See Info node `(magit) How to install the gitman info manual?'."
    (if (string-prefix-p "(gitman)" (car r))
        (man (substring (car r) (length "(gitman)")))
      (apply orig-fun r)))

  (advice-add 'Info-goto-node :around #'Info-goto-node--gitman-for-magit)

  ;; Work with ~/.dotfiles (a bare Git repository)
  (define-advice magit-process-git-arguments (:around (orig-fun &rest r) dotfiles-hack)
    "Temporarily add --git-dir=$HOME/.dotfiles and --work-tree=$HOME for the ~/.dotfiles bare Git repository."
    ;; NOTE Slow down Magit?
    (if (member (expand-file-name default-directory)
                (list (expand-file-name "~/.dotfiles/")
                      (expand-file-name "~/")))
        (let ((magit-git-global-arguments
               (append magit-git-global-arguments
                       (list (format "--work-tree=%s" (expand-file-name "~/"))
                             (format "--git-dir=%s"   (expand-file-name "~/.dotfiles/"))))))
          (apply orig-fun r))
      (apply orig-fun r))))

(use-package vc
  :defer t
  :init
  ;; Don't ask me again
  (setq vc-follow-symlinks t))

(use-package git-gutter
  :ensure t
  :bind (("C-x G"   . git-gutter-mode)
         ("C-x v n" . git-gutter:next-hunk)
         ("C-x v p" . git-gutter:previous-hunk)
         ("C-x v s" . git-gutter:stage-hunk)
         ("C-x v r" . git-gutter:revert-hunk))
  :config
  (setq git-gutter:handled-backends '(git svn))

  (defun chunyang-git-gutter-count-hunks (beg end)
    (let ((number-of-hunks 0))
      (save-excursion
        (goto-char beg)
        (when (ignore-errors (git-gutter:search-here-diffinfo git-gutter:diffinfos))
          (setq number-of-hunks 1))
        (while (let ((old-pt (point)))
                 (git-gutter:next-hunk 1)
                 (and (> (point) old-pt)
                      (<= (point) end)))
          (setq number-of-hunks (+ 1 number-of-hunks))))
      ;; (message "You have %d changes in the region" number-of-hunks)
      number-of-hunks))

  (defun chunyang-git-gutter-apply-on-region (beg end revert-or-stage)
    (let ((git-gutter:ask-p nil)
          (number-of-hunks (chunyang-git-gutter-count-hunks beg end)))
      (goto-char beg)
      (when (ignore-errors (git-gutter:search-here-diffinfo git-gutter:diffinfos))
        (funcall revert-or-stage)
        (sit-for .3)
        (setq number-of-hunks (- number-of-hunks 1)))
      (dotimes (_ number-of-hunks)
        (git-gutter:next-hunk 1)
        (funcall revert-or-stage)
        (sit-for .3))
      (deactivate-mark)
      (git-gutter:update-all-windows)))

  (defun chunyang-git-gutter-revert-region (beg end)
    (interactive "r")
    (chunyang-git-gutter-apply-on-region beg end 'git-gutter:revert-hunk))

  (defun chunyang-git-gutter-stage-region (beg end)
    (interactive "r")
    (chunyang-git-gutter-apply-on-region beg end 'git-gutter:stage-hunk)))

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
  :disabled t                           ; Doesn't work for me any more
  :ensure t
  :defer 10
  :config
  (setq edit-server-new-frame nil)
  (edit-server-start))

;; TODO Find a better (than "It'a All Text!") alternative for Firefox since now
;;      I am moving to Firefox.
(use-package atomic-chrome
  :ensure t                             ; To install its dependencies
  :load-path "~/src/atomic-chrome"
  :config
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com"      . gfm-mode)
          ("emacs-china\\.org" . gfm-mode)))
  (atomic-chrome-start-server))

(use-package ediff
  :defer t
  :config
  ;; (setq ediff-window-setup-function 'ediff-setup-windows-plain
  ;;       ediff-split-window-function 'split-window-horizontally)
  ;; (setq ediff-custom-diff-program "diff"
  ;;       ediff-custom-diff-options "-u")
  )

(use-package server
  :defer 7
  :config
  (unless (server-running-p) (server-start)))

(use-package gh-md             :ensure t :defer t)

(use-package github-notifier
  :load-path "~/src/github-notifier.el"
  :commands github-notifier)

(use-package chunyang-github
  :ensure ghub                          ; Dependency
  :commands (helm-chunyang-github-stars
             helm-chunyang-github-repos))

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
  :ensure t
  :diminish projectile-mode
  :config
  (setq projectile-completion-system chunyang-completion-system
        projectile-mode-line
        '(:eval (if (projectile-project-p)
                    (format " P[%s]"
                            (propertize (projectile-project-name)
                                        'face 'bold))
                  "")))
  (projectile-global-mode))

(use-package helm-projectile
  :disabled t
  :if (eq chunyang-completion-system 'helm)
  :after projectile
  :ensure t
  :config
  (helm-projectile-on)
  (helm-delete-action-from-source
   "Grep in projects `C-s'"
   helm-source-projectile-projects)
  (helm-add-action-to-source
   "Ag in project C-s'"
   'helm-do-ag helm-source-projectile-projects)
  (bind-key "C-s" (defun helm-projectile-do-ag ()
                    (interactive)
                    (helm-exit-and-execute-action #'helm-do-ag))
            helm-projectile-projects-map))

(use-package chunyang-browse-projects
  :disabled t
  :defer t
  :init
  (defvar chunyang-project-list nil)

  (defun chunyang-find-file-hook ()
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root
        (push root chunyang-project-list)
        (delete-dups chunyang-project-list))))

  (add-hook 'find-file-hook #'chunyang-find-file-hook t)

  ;; Assuming desktop-save-mode has been enabled
  (add-to-list 'desktop-globals-to-save 'chunyang-project-list)
  (add-hook 'kill-emacs-hook
            ;; This should run BEFORE desktop saves `chunyang-project-list'
            (defun chunyang-project-clear-non-exist ()
              (cl-delete-if-not #'file-exists-p chunyang-project-list)))

  (setq chunyang-browse-projects-actions
        (helm-make-actions
         "Browse"
         (lambda (root)
           (let ((default-directory root))
             (helm-browse-project nil)))
         "Search `C-s'"
         #'helm-grep-git-1
         "Magit `M-g'"
         #'magit-status
         "cd with Terminal.app `C-d'"
         #'chunyang-cd-in-Terminal.app))

  (setq chunyang-browse-projects-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map helm-map)
          (define-key map "\C-s"
            (defun chunyang-run-git-grep ()
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action 'helm-grep-git-1))))
          (define-key map "\M-g"
            (defun chunyang-run-magit ()
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action 'magit-status))))
          (define-key map "\C-d"
            (defun chunyang-run-cd-in-Terminal.app ()
              (interactive)
              (with-helm-alive-p
                (helm-exit-and-execute-action 'chunyang-cd-in-Terminal.app))))
          map))

  (setq chunyang-browse-projects-source
        (helm-build-sync-source "helm projects"
          :candidates 'chunyang-project-list
          :keymap chunyang-browse-projects-map
          :action chunyang-browse-projects-actions))

  (setq helm-mini-default-sources
        '(helm-source-buffers-list
          helm-source-recentf
          chunyang-browse-projects-source
          helm-source-buffer-not-found))

  (defun chunyang-browse-projects ()
    (interactive)
    (helm :sources 'chunyang-browse-projects-source
          :buffer "*Helm Projects*"))

  (define-key global-map "\C-xp" #'chunyang-browse-projects)
  (define-key global-map "\C-cp" #'chunyang-browse-projects))


;;; Web & IRC & Email & RSS

;; I could not download mails from QQMail with offlineimap and mbsync
;; successfully, so I give up reading mails from Emacs.  But I still can and
;; need to write and send from Emacs.
(setq user-full-name       "Chunyang Xu"
      user-mail-address    "mail@xuchunyang.me"
      smtpmail-smtp-server "smtp.exmail.qq.com"
      send-mail-function   'smtpmail-send-it)
;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq message-directory (locate-user-emacs-file "var/Mail"))

(use-package mu4e
  :preface
  ;; On Mac OS, I installed mu manually since MacPorts failed for some reason
  ;; thus I have to setup `load-path' for mu4e
  (when *is-mac*
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))
  :commands mu4e
  :config
  (setq
   mu4e-maildir       "~/Maildir"         ;; top-level Maildir
   mu4e-sent-folder   "/Sent Messages"    ;; folder for sent messages
   mu4e-drafts-folder "/Drafts"           ;; unfinished messages
   mu4e-trash-folder  "/Deleted Messages" ;; trashed messages
   mu4e-refile-folder "/Archives")
  (setq mu4e-sent-messages-behavior 'delete)

  (setq mu4e-get-mail-command "offlineimap"
        ;; mu4e-update-interval 300
        )

  ;; By default, mu4e use `ido-completing-read'
  (when (bound-and-true-p helm-mode)
    (setq mu4e-completing-read-function 'completing-read))

  (require 'org-mu4e))

(use-package gnus
  :defer t
  :init
  (setq gnus-directory (locate-user-emacs-file "var/gnus/"))
  (setq gnus-select-method
        '(nnmaildir "Mail"
                    (directory "~/Maildir")
                    (directory-files nnheader-directory-files-safe)
                    (get-new-mail nil)))
  (with-eval-after-load "ace-link"
    (with-eval-after-load "gnus-art"
      (bind-key "o" 'ace-link-gnus gnus-article-mode-map))))

(use-package erc
  :preface
  (defun chat ()
    "Chat in IRC with ERC."
    (interactive)
    (erc :server "irc.freenode.net"
         :port "6667"
         :nick erc-nick
         :password erc-password))
  :init
  ;; Join the #emacs channels whenever connecting to Freenode.
  (setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
  ;; Shorten buffer name (e.g., "freenode" instead of "irc.freenode.net:6667")
  (setq erc-rename-buffers t)
  :defer t)

(use-package newsticker
  :disabled t                           ; Only for reference
  :defer t
  :init
  ;; 必须在 (require 'newsticker) 之前
  (setq newsticker-retrieval-interval 0)  ; 不在后台自动更新

  (setq newsticker-url-list-defaults nil
        newsticker-url-list '(("Topic" "https://emacs-china.org/latest.rss")
                              ("Post"  "https://emacs-china.org/posts.rss")
                              ("HN"    "https://news.ycombinator.com/rss")
                              ("blog"  "https://xuchunyang.me/rss.xml"))))

(use-package sx
  :ensure t :defer t)

(use-package google-this
  :disabled t
  :ensure t
  :diminish google-this-mode
  :preface (defvar google-this-keybind (kbd "C-c G"))
  :init (google-this-mode))

(use-package devdocs
  :ensure t
  :commands devdocs-search)

(defun web-search (prefix)
  "Web search with s (see URL `https://github.com/zquestz/s').
Called with a prefix arg set search provider (default Google)."
  (interactive "P")
  (let* ((provider
          (if prefix
              (completing-read
               "Set search provider: "
               (split-string (shell-command-to-string "s -l") "\n" t) nil t)))
         (initial
          (or (if (region-active-p)
                  (buffer-substring-no-properties
                   (region-beginning)
                   (region-end)))
              (thing-at-point 'symbol)
              (thing-at-point 'word)))
         (query (shell-quote-argument (read-string "Web Search: " initial))))
    (call-process-shell-command
     (if provider
         (format "s -p %s %s" provider query)
       (format "s %s" query)) nil)))

(bind-key "M-s M-s" #'web-search)

(use-package emms
  :disabled t
  :ensure t
  :defer t
  :config
  (add-to-list 'Info-directory-list "~/src/emms/doc")

  (setq emms-mode-line-icon-color "purple")

  (require 'emms-setup)
  (emms-all)
  (emms-playing-time -1)
  (setq emms-player-list '(emms-player-mplayer)
        emms-source-file-default-directory "~/Music/网易云音乐"
        emms-source-file-gnu-find "gfind")

  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))

  (defun chunyang-emms-indicate-seek (_sec)
    (let* ((total-playing-time (emms-track-get
                                (emms-playlist-current-selected-track)
                                'info-playing-time))
           (elapsed/total (/ (* 100 emms-playing-time) total-playing-time)))
      (with-temp-message (format "[%-100s] %2d%%"
                                 (make-string elapsed/total ?=)
                                 elapsed/total)
        (sit-for 2))))

  (add-hook 'emms-player-seeked-functions #'chunyang-emms-indicate-seek 'append))

(use-package bongo
  :ensure t
  :defer t)

;;; Dictionary
(use-package youdao-dictionary
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search)
         ("C-c Y" . youdao-dictionary-search-at-point+)))

(use-package osx-dictionary
  :if *is-mac*
  :load-path "~/src/osx-dictionary.el"
  :bind ("C-c d" . osx-dictionary-search-pointer))

(use-package bing-dict
  :ensure t
  :defer t)

(use-package echo
  :commands echo-mode)

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


;;; Shell (including shell-command, shell, term and Eshell)

(use-package term
  :commands term
  :config
  ;; Allow more lines before truncating
  (setq term-buffer-maximum-size 10240)

  ;; Respect my own M-x
  (define-key term-raw-escape-map [?\M-x] (lookup-key global-map [?\M-x]))

  ;; C-c C-j   term-line-mode
  ;; C-c C-k   term-char-mode
  ;;
  ;; However, It's hard to remember and distinguish them, so just use C-c C-j to
  ;; toggle these two modes.
  (define-key term-mode-map [?\C-c ?\C-j] 'term-char-mode))

(use-package shell-pop
  :ensure t
  :commands shell-pop)

(use-package eshell-z
  :commands eshell-z
  :load-path "~/src/eshell-z")

(use-package eshell
  :defer t
  :preface
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
  :bind ("C-x m" . eshell)              ; 'C-x m' runs `compose-mail' by default
  :config
  (setq eshell-history-size 5000)       ; Same as $HISTSIZE
  (setq eshell-hist-ignoredups t)       ; make the input history more bash-like
  ;; (setq eshell-banner-message
  ;;       '(concat (shell-command-to-string "fortune") "\n"))

  ;; Visual commands like top(1) and vi(1)
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (setq eshell-destroy-buffer-when-process-dies t)

  ;; If t, it will make `term' use C-x as escape key, instead of the default C-c
  (setq eshell-escape-control-x nil)

  (add-hook 'eshell-mode-hook
            (defun chunyang-eshell-mode-setup ()
              ;; Setup Plan9 smart shell
              ;; (require 'em-smart)
              ;; (eshell-smart-initialize)
              (bind-keys :map eshell-mode-map
                         ("C-c C-q" . eshell-kill-process)
                         ("C-c ."   . eshell-insert-last-arg))
              (eshell/export "EDITOR=emacsclient -n")
              (eshell/export "VISUAL=emacsclient -n")
              ;; Disable scroll, see
              ;; https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command
              (remove-hook 'eshell-output-filter-functions
                           'eshell-postoutput-scroll-to-bottom)

              (require 'eshell-z)))

  ;; Needed at least for `eshell-git-prompt'?
  (setq eshell-highlight-prompt nil)

  (use-package eshell-git-prompt
    :ensure t
    :config (eshell-git-prompt-use-theme 'powerline)))



(use-package eshell-did-you-mean
  :disabled t
  :load-path "~/src/eshell-did-you-mean"
  :defer t
  :init
  (autoload 'eshell-did-you-mean-setup "eshell-did-you-mean")
  (with-eval-after-load 'eshell
    (eshell-did-you-mean-setup)))


;;; Emacs + Shell

;; 1) Emacs should load `chunyang-shell.el'
;; 2) Shell in external Terminal should load misc/linux.sh or misc/mac.sh

(use-package chunyang-shell)

;; For "emacsclient --eval EXPR". In EXPR, one has to use `setq' to modify this
;; variable temporarily, for example,
;;
;; emacsclient --eval "(progn (setq server-eval-and-how-to-print 'buffer) (org-agenda-list))"
;;
;; `let' will not work.
(defcustom server-eval-and-how-to-print 'supress
  "Custom how to print in `server-eval-and-print'."
  :type '(choice
          (const :tag "Default (that is, print result of evaluation)" nil)
          (const :tag "Supress everything" 'supress)
          (const :tag "Print the current buffer" 'buffer)))

(define-advice server-eval-and-print (:around (&rest r) how-to-print)
  "Decide how to print in `server-eval-and-print', according to `server-eval-and-how-to-print'."
  (let ((server-eval-and-how-to-print
         server-eval-and-how-to-print))
    (seq-let (orig-fun expr proc) r
      (let ((v (with-local-quit (eval (car (read-from-string expr))))))
        (pcase server-eval-and-how-to-print
          ('supress
           nil)
          ('buffer
           (when proc
             (require 'e2ansi)
             (server-reply-print
              (server-quote-arg
               (e2ansi-string-to-ansi (buffer-string)))
              proc)))
          (_
           (when proc
             (with-temp-buffer
               (let ((standard-output (current-buffer)))
                 (pp v)
                 (let ((text (buffer-substring-no-properties
                              (point-min) (point-max))))
                   (server-reply-print (server-quote-arg text) proc)))))))))))


;;; Org mode

;; (require 'chunyang-org)

;; How to install Org 9.0 from Git?
;; - git clone ...
;; - git checkout -b v9.0 release_9.0
;; - make
;; - setup `load-path' and `Info-directory-list'
;; - load the autoload file `org-loaddefs.el'

(use-package org
  :defer t
  ;; Install Org 9.0 from Git
  :load-path "~/src/org-mode/lisp"
  :load-path "~/src/org-mode/contrib/lisp"
  :init
  ;; Yes, since I am installing Org from Git, not package.el or built-in
  (require 'org-loaddefs)
  (add-to-list 'Info-directory-list "~/src/org-mode/doc/")

  (setq org-agenda-files (list "~/Sync/chunyang.org"))
  (setq org-default-notes-file "~/Sync/chunyang.org")
  (define-key global-map [?\C-c ?c] #'org-capture)
  (define-key global-map [?\C-c ?a] #'org-agenda)

  (setq org-agenda-span 'day)

  ;; This will loads org.el but I need it from the begining
  ;; (require 'org-protocol)

  ;; TODO: Create and load autoloads file for contrib/lisp
  (when *is-mac*
    (autoload 'org-mac-grab-link "org-mac-link"))

  (setq org-edit-src-content-indentation 0)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t))))

(use-package grab-mac-link
  ;; Notice that `:ensure' has a higher priority than `:if'
  ;; :if *is-mac*
  :disabled (not *is-mac*)
  :load-path "~/src/grab-mac-link"
  :ensure t
  :bind ("C-c L" . grab-mac-link))

(use-package orglink
  :disabled t                           ; I like this package, but it requires
                                        ; org-8.3, while I am installing org
                                        ; manuallly and in Emacs-25 the built-in
                                        ; version of org is org-8.2, so If I
                                        ; install this package via ELPA, org
                                        ; will also be install as dependency,
                                        ; this should be OK in fact, but I just
                                        ; don't want this.
  :ensure t
  ;; :diminish orglink-mode
  ;; NOTE The problem is: it will slow down Emacs startup
  ;; :init (global-orglink-mode)
  :defer t)


(use-package habitica
  :disabled t
  :ensure t
  :defer t)



;;; C

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-hook 'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-irony))
  :config
  (define-advice company-irony--post-completion (:override (candidate) prefer-gnu-c-style)
    "WARNING: This is a bad idea!  Hacking 20160826.56 from MELPA."
    (when candidate
      (let ((point-before-post-complete (point)))
        (if (irony-snippet-available-p)
            (irony-completion-post-complete candidate)
          (let ((str (irony-completion-post-comp-str candidate)))
            ;; Prefer GNU C style by adding one space after function name (2016-10-24 by xcy)
            (unless (string-empty-p str)
              (insert " "))
            (insert str)
            (company-template-c-like-templatify str)))
        (unless (eq (point) point-before-post-complete)
          (setq this-command 'self-insert-command))))))

(use-package irony-eldoc                ; Note: this does not work very well
  :ensure t
  :defer t
  :init (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck-irony
  :ensure t
  :defer t
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; GTK+

(use-package cc-mode
  ;; Tips:
  ;;
  ;; - (info "ccmode") has some practical tips
  ;; - C-M-a/e and M-a/e understands functions and statements, it's cool

  :if (eq system-type 'gnu/linux)
  :preface
  (defun devhelp (symbol)
    (interactive (list (current-word)))
    (and symbol
         (start-process "devhelp" nil "devhelp" "-s" symbol)))
  (use-package gtk-look
    :ensure t
    :defer t
    :init
    (defvar helm-gtk-lookup-symbol-input-history nil)
    (defun helm-gtk-lookup-symbol ()
      (interactive)
      (helm :sources
            (helm-build-sync-source "gtk-doc"
              :candidates (gtk-lookup-cache-init)
              :action
              '(("View gtk-doc in eww" .
                 (lambda (entry)
                   (eww-browse-url
                    (concat "file://" (nth 1 entry) (nth 0 entry)))))))
            :input (when (memq major-mode '(c-mode))
                     (current-word))
            :buffer "*helm gtk-doc*"
            :history 'helm-gtk-lookup-symbol-input-history))
    :config
    ;; Prefer EWW (`browse-url' prefers system's default browser)
    (setq browse-url-browser-function
          '(("file:///usr/share" . eww-browse-url)
            ("." . browse-url-default-browser))))
  (defun chunyang-c-mode-setup-gtk ()
    (define-key c-mode-map "\C-h." 'gtk-lookup-symbol)
    (define-key c-mode-map "\C-c\C-c" 'recompile))
  :defer t
  :init
  (add-hook 'c-mode-hook 'chunyang-c-mode-setup-gtk)
  ;; Turn on Auto-newline and hungry-delete-key, they are adviced by cc-mode
  ;; manual, let me try them for a while. BTW, 'a' and 'h' will be indicated in
  ;; the mode-line, such as C/lah.  BTW, 'l' stands for electric keys, use C-c
  ;; C-l to toggle it.
  (add-hook 'c-mode-common-hook 'c-toggle-auto-hungry-state))


;;; Common Lisp

(use-package slime
  :ensure t
  :defer t
  :init (setq inferior-lisp-program "sbcl"))


;;; Ruby

(use-package ruby-mode
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode 'superword-mode))

(use-package inf-ruby
  :ensure t
  ;; `package.el' does the setup via autoload
  :defer t)

(use-package robe
  :ensure t
  :after ruby-mode
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package ruby-tools
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook 'ruby-tools-mode))


;; For the Programming Languages course
(use-package sml-mode
  :ensure t
  :defer t)

(use-package sml-eldoc
  :commands sml-eldoc-turn-on
  :init (add-hook 'sml-mode-hook 'sml-eldoc-turn-on))

;; mlton's Emacs tools (including mlb-mode, background build mode and def-use-mode)
(use-package mlton
  :load-path "~/Downloads/mlton-master/ide/emacs"
  :defer t
  :init
  (autoload 'esml-mlb-mode "esml-mlb-mode")
  (add-to-list 'auto-mode-alist '("\\.mlb\\'" . esml-mlb-mode))
  (autoload 'def-use-mode "def-use-mode" "Highlighting and navigating definitions and uses")
  (autoload 'bg-build-mode "bg-build-mode" "Build on the background")
  (with-eval-after-load 'compile
    (add-to-list
     'compilation-error-regexp-alist
     '("^\\(Warning\\|Error\\): \\(.+\\) \\([0-9]+\\)\\.\\([0-9]+\\)\\.$"
       2 3 4))))

(use-package ob-sml
  :ensure t
  :defer t
  :init (with-eval-after-load 'org
          (require 'ob-sml)))

(use-package skalpel
  :disabled t
  :load-path "~/skalpel/front-ends/emacs/"
  :init
  (defvar skalpel-emacs-directory "~/skalpel/front-ends/emacs/")
  (defvar skalpel-bin-directory "~/skalpel/analysis-engines/standard-ml/bin/")
  (defvar skalpel-lib-directory "~/skalpel/lib/")
  (defvar skalpel-sources-directory "~/skalpel/analysis-engines/standard-ml/")
  (with-eval-after-load 'sml-mode
    (load "skalpel-config.el")))

;; [Don't copy-paste on the Extra Practice Problems page](https://www.coursera.org/learn/programming-languages/discussions/weeks/2/threads/_k7m9FTgEeaslgpY-vgBqw
(defun convert-MathJax-Unicode-to-ASCII (beg end)
  "Convert [0-9a-zA-Z] in Unicode to their ASCII in the region."
  (interactive "r")
  (insert (mapconcat (lambda (c)
                       (char-to-string
                        (cond
                         ((<= ?𝟶 c ?𝟿) (+ ?0 (- c ?𝟶)))
                         ((<= ?𝚊 c ?𝚣) (+ ?a (- c ?𝚊)))
                         ((<= ?𝙰 c ?𝚉) (+ ?A (- c ?𝙰)))
                         (t c))))
                     (delete-and-extract-region beg end) "")))
;; Testing
;;
;; Before:
;; EXAMPLE: 𝚞𝚗𝚏𝚘𝚕𝚍 (𝚏𝚗 𝚡 => 𝚒𝚏 𝚡 > 𝟹 𝚝𝚑𝚎𝚗 𝙽𝙾𝙽𝙴 𝚎𝚕𝚜𝚎 𝚂𝙾𝙼𝙴 (𝚡 + 𝟷, 𝚡)) 𝟶 = [𝟶, 𝟷, 𝟸, 𝟹]
;; After:
;; EXAMPLE: unfold (fn x => if x > 3 then NONE else SOME (x + 1, x)) 0 = [0, 1, 2, 3]

(use-package geiser
  :ensure t
  :defer t
  :init
  (setq geiser-default-implementation 'racket)
  (setq geiser-active-implementations '(racket))

  ;; Important keys:
  ;; C-c C-z - Switch between source and REPL
  ;; C-z C-a - Switch to REPL with current module
  (defun chunyang-geiser-setup ()
    (bind-keys :map geiser-mode-map
               ("C-h ." . geiser-doc-symbol-at-point)
               ("C-h C-." . geiser-doc-look-up-manual)))
  (add-hook 'geiser-mode-hook #'chunyang-geiser-setup)

  ;; Yes, use ParEdit in the REPL too
  (add-hook 'geiser-repl-mode-hook #'paredit-mode))

(add-hook 'scheme-mode-hook #'paredit-mode)

(use-package racket-mode
  :disabled t
  :ensure t
  :defer t
  :init
  ;; Doing this is because geiser sets *.rkt as scheme mode and overrides
  ;; racket-mode
  (push '("\\.rkt[dl]?\\'" . racket-mode) auto-mode-alist))


;;; Web

(use-package css-mode
  :defer t
  :init
  (add-hook 'css-mode-hook
            (defun chunyang-css-setup ()
              ;; FIXME Not working?
              ;; (setq indent-tabs-mode t)
              (setq css-indent-offset 2))))

;; TODO Try this package (examples, documentation)
(use-package web-server
  :ensure t
  :defer t)


;;; Python

(use-package python
  :defer t
  :init
  (setq python-shell-interpreter "python3"
        python-shell-completion-native-enable nil))


;; OCaml
(use-package OCaml
  :ensure tuareg
  :preface
  ;; 官方的 Manual （竟然）提供了 Info 格式 <http://caml.inria.fr/pub/docs/manual-ocaml/>
  (add-to-list 'Info-directory-list "~/Documents/ocaml-4.03-info-manual")
  :defer t)


;; shen <http://shenlanguage.org/>
(use-package shen-mode
  :ensure t
  :defer t)

(use-package shen-elisp
  :ensure t
  :defer t)


;; AppleScript
(use-package applescript-mode
  ;; Needs build with ns (cocoa) support
  :if (and (eq 'darwin system-type) (eq 'ns window-system))
  :ensure t
  :mode ("\\.applescript\\'" "\\.scpt\\'"))


;;; Math

(use-package Maxima
  :disabled t
  :load-path "/usr/local/Cellar/maxima/5.37.2/share/maxima/5.37.2/emacs"
  :mode ("\\.ma[cx]" . maxima-mode)
  :init
  (autoload 'maxima-mode "maxima"  "Maxima mode"                            t)
  (autoload 'imaxima     "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima      "maxima"  "Maxima interaction"                     t)
  (autoload 'imath-mode  "imath"   "Imath mode for math formula input"      t)
  (setq imaxima-use-maxima-mode-flag t))


;;; Misc

;; Socks5 proxy setting of `url.el'
;; (setq url-gateway-method 'socks
;;       socks-server '("Default server" "127.0.0.1" 1080 5))

(use-package opencc                     ; WIP
  :disabled t
  :load-path "~/src/emacs-opencc"
  :commands (opencc-use-api opencc-use-cli opencc)
  :defer t)

(use-package chunyang-blog
  :disabled t
  :load-path "~/src/blog"
  :commands chunyang-blog-publish)

(use-package number-to-word             ; WIP
  :disabled t
  :defer t
  :load-path "~/src/number-to-word")

;; (use-package nginx-mode :ensure t :defer t)

(use-package ascii-art-to-unicode
  :ensure t
  :defer t)

(use-package sl
  :ensure t
  :load-path "~/src/sl.el"
  :commands (sl sl-little sl-forever sl-little-forever sl-screen-saver))

(use-package time-stamp                 ; Built-in
  :defer t
  :init (add-hook 'before-save-hook 'time-stamp))

(use-package autoinsert
  :disabled t
  :defer t
  :config
  (add-to-list 'auto-insert-alist "TODO Add some Python if it is really useful"))

(use-package restart-emacs :ensure t :defer t)

(use-package e2ansi                     ; Provide Syntax Highlight for shell by
                                        ; Emacs.  This is very cool.
  :ensure t
  :load-path "~/src/e2ansi"
  :defer t
  :config
  ;; On my Ubuntu Box, I use Gnome-terminal with its default profile/theme, I
  ;; can't see the output of `e2ansi' clearly without changing
  ;; `e2ansi-background-mode' to `dark'.
  (when *is-gnu-linux*
    (setq e2ansi-background-mode 'dark)))


;;; IM

(use-package gitter
  :load-path "~/src/gitter.el"
  :ensure t
  :defer t
  :commands gitter
  :init
  (setq gitter--debug t))


;;; TODO

(use-package language-detection
  :ensure t
  ;; TODO Try this package
  :defer t)


;;; Emacs

;; FIXME: In case Emacs can't find the source (for example, installing Emacs via
;;        MacPorts). Remove it someday
(setq source-directory "~/src/emacs/")


;;; Customization

;; Load custom-file in the end to prevent it loads some package.

(use-package cus-edit
  :defer t
  :init
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file :no-error :no-message))

;;; init.el ends here
