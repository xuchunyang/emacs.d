;;; init.el --- Xu Chunyang's Emacs Configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020  Xu Chunyang

;; Author: Xu Chunyang
;; URL: https://github.com/xuchunyang/emacs.d

;;; Code:


;;; Startup

;; If it's not me, skip loading the rest
(unless (equal user-login-name "xcy")
  (warn "Please don't load Xu Chunyang's init file, it won't work for you")
  (with-current-buffer " *load*"
    (goto-char (point-max))))

;; Custom
(setq custom-file "~/.emacs.d/custom.el")
(setq message-log-max 10000)

;; Emacs should have them, see Bug#33576
(defun version> (v1 v2) (version< v2 v1))
(defun version>= (v1 v2) (version<= v2 v1))


;;; Package Manager

(when (version<= emacs-version "27")
  (require 'package)
  (setq package-user-dir (concat "~/.emacs.d/elpa-" emacs-version))
  (setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (setq package-archives '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
                           ("melpa" . "https://elpa.emacs-china.org/melpa/")))
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
  (setq package-archives '(("gnu"   . "https://mirrors.cloud.tencent.com/elpa/gnu/")
                           ("melpa" . "https://mirrors.cloud.tencent.com/elpa/melpa/")))
  (package-initialize))


;;; `use-package'

(unless (fboundp 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :config
  (setq use-package-verbose 'errors)
  (setq use-package-expand-minimally t)

  (defmacro chunyang-use-package-keywords-add (keyword)
    "Add new keyword as placeholder."
    `(progn
       (add-to-list 'use-package-keywords ,keyword 'append)
       (defun ,(intern (format "use-package-normalize/%s" keyword)) (&rest _))
       (defun ,(intern (format "use-package-handler/%s" keyword)) (&rest _))))

  (chunyang-use-package-keywords-add :about)
  (chunyang-use-package-keywords-add :homepage)
  (chunyang-use-package-keywords-add :info)
  (chunyang-use-package-keywords-add :notes))

(use-package diminish :ensure t)


;;; Extra `load-path'

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;;; Require helper libraries

(require 'subr-x)
(require 'seq)
(require 'rx)

(use-package dash
  :ensure t
  :config
  ;; Will also make `it', `acc' into variable face everywhere
  (dash-enable-font-lock))


;;; Initialization

;; Disable the site default settings. See (info "(emacs) Init File")
(setq inhibit-default-init t)

;; Don't show "For information about GNU Emacs and the GNU system, type C-h
;; C-a." after startup
(setq inhibit-startup-echo-area-message "xcy")

;; Use `fundamental-mode' to reduce startup time
(setq initial-major-mode 'lisp-interaction-mode)

;; `lexical-binding' is on in *scratch* since 27.1
(when (version< emacs-version "27.1")
  (add-hook 'emacs-startup-hook
            (defun chunyang-enable-lexical-binding ()
              (let ((buffer (get-buffer "*scratch*")))
                (when buffer
                  (with-current-buffer buffer
                    (setq lexical-binding t)))))))

(use-package chunyang-scratch
  :preface
  (defun chunyang-scratch-save ()
    (ignore-errors
      (with-current-buffer "*scratch*"
        (write-region nil nil "~/.emacs.d/var/scratch"))))

  (defun chunyang-scratch-restore ()
    (let ((f "~/.emacs.d/var/scratch"))
      (when (file-exists-p f)
        (with-current-buffer "*scratch*"
          (insert-file-contents f)
          (set-buffer-modified-p nil)))))
  :init
  (add-hook 'kill-emacs-hook #'chunyang-scratch-save)
  (add-hook 'after-init-hook #'chunyang-scratch-restore)
  ;; This is not a real package so don't load it
  :defer t)

;; Load personal secrets
(load (expand-file-name user-login-name user-emacs-directory) 'no-error)

;; Make ~/.emacs.d clean
(use-package no-littering
  :ensure t)

;; See also `bidi-paragraph-direction'; setting that non-nil might
;; speed up redisplay.
(setq bidi-paragraph-direction 'left-to-right)


;;; macOS

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-mac-port* (boundp 'mac-carbon-version-string))

(use-package ns-win
  :if *is-mac*
  :no-require t
  :defer t
  :init
  ;; Anne Pro 2
  (setq mac-command-modifier 'meta
        mac-option-modifier 'meta)

  ;; 自带键盘
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control)

  :preface
  (defun chunyang-toggle-keyboard ()
    (interactive)
    (pcase mac-option-modifier
      ('meta
       (message "anne pro 2 => builtin")
       (setq mac-option-modifier 'control))
      ('control
       (message "builtin => anne pro 2")
       (setq mac-option-modifier 'meta)))))

(use-package mac-win
  :disabled
  :if *is-mac-port*
  :config
  (mac-auto-ascii-mode)

  ;; This is annoying
  ;; (key-binding (kbd "<C-tab>"))
  ;; => mac-next-tab-or-toggle-tab-bar
  (unbind-key "<C-tab>"))

;; [[https://emacs-china.org/t/topic/5507][Mac 下给 Emacs 设置 PATH 和 exec-path - Emacs-general - Emacs China]]
;; NOTE: When PATH is changed, run the following command
;; $ sh -c 'printf "%s" "$PATH"' > .path
(when *is-mac*
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

(use-package chunyang-mac
  :if *is-mac*
  :commands (chunyang-mac-Terminal-send-region
             chunyang-mac-Terminal-cd
             chunyang-mac-iTerm-send-region
             chunyang-mac-iTerm-send-string
             chunyang-mac-iTerm-cd
             chunyang-mac-Finder-reveal
             chunyang-mac-edit-file-tags
             chunyang-mac-search-tags
             helm-chunyang-mac-tags
             chunyang-chrome-refresh
             chunyang-chrome-url
             chunyang-mac-app-running-p
             chunyang-mac-switch-to-app
             chunyang-mac-switch-input-source
             chunyang-mac-toggle-theme
             chunyang-chrome-screenshot
             chunyang-chrome-switch-tab))

(use-package chrome-cli
  :if *is-mac*
  :commands chrome-cli-switch-to-tab)

(use-package iterm2
  :if *is-mac*
  :commands (iterm2-send-region
             iterm2-cd))

(use-package finda
  :if *is-mac*
  :homepage https://keminglabs.com/finda/
  :about Open apps, files, Emacs buffers, Chrome tabs/history etc
  :defer 2
  :config
  (require 'server)
  (or (server-running-p) (server-mode)))

(use-package omnifocus
  :commands omnifocus-capture)

(use-package helm-osx-app
  :about "Launch macOS apps"
  :load-path "~/src/helm-osx-app"
  :commands helm-osx-app)

(use-package helm-chrome-control
  :about "Control Chrome tabs"
  :load-path "~/src/helm-chrome-control"
  :commands helm-chrome-control)


;;; User Interface
(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))
(when (bound-and-true-p scroll-bar-mode)
  (scroll-bar-mode -1))

(setq inhibit-startup-screen t)

(setq ring-bell-function #'ignore)

(fset 'yes-or-no-p #'y-or-n-p)

(setq echo-keystrokes 0.6)              ; 默认 1 秒，更快地显示未完成地按键

(defun chunyang-frame-transparency-adjust ()
  "Adjust current frame's transparency using <up> and <down>."
  (declare (interactive-only "Use `set-frame-parameter' instead."))
  (interactive)
  ;; If `alpha' is not a number in [0, 100], reset to 100
  (pcase (frame-parameter nil 'alpha)
    ((and (pred numberp) n (guard (<= 0 n 100))))
    (_ (setf (frame-parameter nil 'alpha) 100)))
  (while (pcase (read-key (format "%2d%%  Press <up> and <down> to adjust"
                                  (frame-parameter nil 'alpha)))
           ((or (and 'up   (let new-alpha (1+ (frame-parameter nil 'alpha))))
                (and 'down (let new-alpha (1- (frame-parameter nil 'alpha)))))
            (when (<= 0 new-alpha 100)
              (setf (frame-parameter nil 'alpha) new-alpha))
            t))))

;; (column-number-mode)
(size-indication-mode)

(defconst chunyang-mode-line-format
  '(" %+  "
    mode-line-buffer-identification
    "   "
    mode-line-position
    "   "
    mode-line-modes
    mode-line-misc-info)
  "A simple and clean mode-line.")
;; (substring-no-properties (format-mode-line chunyang-mode-line-format))
;; ;; =>
;; " *  init.el         3% of 153k (189,0)     (Emacs-Lisp) "

(setq-default mode-line-format chunyang-mode-line-format)
;; To restore
;; (setq-default mode-line-format (eval (car (get 'mode-line-format 'standard-value))))

(defun chunyang-toggle-mode-or-header-line ()
  "Toggle between mode and header line for displaying the status line."
  (interactive)
  (if (default-value 'header-line-format)
      (setq-default header-line-format nil
                    mode-line-format chunyang-mode-line-format)
    (setq-default mode-line-format nil
                  header-line-format chunyang-mode-line-format)))

(use-package spaceline
  :disabled
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; Fonts
(when (memq window-system '(mac ns))
  (set-face-attribute 'default nil :font "Source Code Pro-13")

  (setq face-font-rescale-alist `(("STkaiti" . ,(/ 16.0 13))))
  (set-fontset-font t 'han      (font-spec :family "STkaiti"))
  (set-fontset-font t 'cjk-misc (font-spec :family "STkaiti")))

(defun chunyang-font-size-adjust ()
  "Adjust font size."
  (interactive)
  (cl-flet ((get-size () (font-get (face-attribute 'default :font) :size))
            (set-size (size)
                      (set-face-attribute
                       'default nil
                       :font
                       (format "%s-%d"
                               (font-get (face-attribute 'default :font) :family)
                               size))
                      t))
    (while (pcase (read-key
                   (format
                    "use <up> and <down> to adjust (current %d), C-g to quit"
                    (get-size)))
             ('up   (set-size (1+ (get-size))))
             ('down (set-size (1- (get-size))))
             (?0    (set-size 13))))))

;; Theme
(use-package spacemacs-theme
  :ensure t
  :no-require t                      ; Silence byte-compiling warnings
  :defer t
  :init (setq spacemacs-theme-comment-bg nil
              spacemacs-theme-org-height nil))

(use-package tomorrow-theme
  :ensure color-theme-sanityinc-tomorrow
  :defer t
  :init (load-theme 'sanityinc-tomorrow-eighties 'no-comfirm))

(use-package tangotango-theme
  :ensure t
  :no-require t
  :defer t)

;; Don't translate quote in `message' and `format-message'
(setq text-quoting-style 'grave)

;; Remove the default italics for C-x 8 RET, see the comment of
;; `mule--ucs-names-annotation'.
(custom-set-faces '(completions-annotations ((t))))

(use-package frame-tabs
  :about Show buffer tabs in side window
  :ensure t
  :defer t
  :preface
  (defun chunyang-frame-tabs-filter (buffer _frame)
    (let ((name (buffer-name buffer)))
      (unless (or (eq (aref name 0) ?\s)
                  (string-match-p "\\`\\*helm" name))
        name)))
  :config
  (setq frame-tabs-filter-function #'chunyang-frame-tabs-filter))


;;; Histroy

(setq history-length 100)
(setq history-delete-duplicates t)

(use-package savehist                   ; Minibuffer history
  :config
  (setq savehist-additional-variables '(extended-command-history
                                        Info-history-list
                                        ivy-views))
  (savehist-mode))

(use-package recentf                    ; Recent files
  :preface
  (defun chunyang-recentf-save-list-for-alfred ()
    "Save the recent list as JSON file for Alfred Script Filter.
See URL `https://www.alfredapp.com/help/workflows/inputs/script-filter/json/'."
    (require 'json)
    (when (and recentf-mode recentf-list)
      (write-region
       (json-encode-list
        `((items
           ,@(mapcar
              (lambda (f)
                (let ((abbrev (abbreviate-file-name f))
                      (onlyname (file-name-nondirectory f)))
                  `((uid . ,abbrev)
                    (type . "file")
                    (title . ,onlyname)
                    (subtitle . ,abbrev)
                    (arg . ,abbrev)
                    (autocomplete . ,onlyname)
                    (icon . ((type . "fileicon")
                             (path . ,abbrev))))))
              recentf-list))))
       nil "~/.recentf-alfred.json" nil 'silent)))
  :config
  (setq recentf-max-saved-items 512
        recentf-exclude '("/\\.git/.*\\'"      ; Git contents
                          "/\\.emacs\\.d/elpa" ; ELPA
                          "/\\.emacs\\.d/etc/"
                          "/\\.emacs\\.d/var/"
                          "-autoloads\\.el\\'"
                          "\\.elc\\'"
                          "/TAGS\\'"))
  (recentf-mode)
  (add-hook 'kill-emacs-hook #'chunyang-recentf-save-list-for-alfred 'append))

(use-package bookmark
  :defer t
  :config
  ;; Save bookmark when Emacs is killed (this is the default)
  (setq bookmark-save-flag t))

(use-package saveplace                  ; Save point position in files
  :config (save-place-mode))


;;; Minibuffer

(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode)

;; Give useful pormpt during M-! (`shell-command') etc
(use-package prompt-watcher
  :preface
  (defun prompt-watcher ()
    (let ((prompt-fn
           (lambda (prompt)
             (let ((inhibit-read-only t)
                   (props (text-properties-at (point-min))))
               (erase-buffer)
               (insert prompt)
               (set-text-properties (point-min) (point-max) props)))))
      (pcase (list this-command (minibuffer-prompt) (and current-prefix-arg t))
        ('(shell-command-on-region "Shell command on region: " t)
         (funcall prompt-fn "Shell command on region and replace: "))
        ('(shell-command "Shell command: " t)
         (funcall prompt-fn "Shell command and insert output: "))
        ('(eshell-command "Emacs shell command: " t)
         (funcall prompt-fn "Emacs shell command and insert output: "))
        ('(async-shell-command "Async shell command: " t)
         (funcall prompt-fn "Async shell command and insert output: "))
        (`(pp-eval-expression "Eval: " ,_)
         (funcall prompt-fn "Pp Eval: ")))))

  (define-minor-mode prompt-watcher-mode
    "Watch the minibuffer prompt and customize if asking."
    :global t
    (if prompt-watcher-mode
        (add-hook 'minibuffer-setup-hook #'prompt-watcher)
      (remove-hook 'minibuffer-setup-hook #'prompt-watcher)))
  :init (prompt-watcher-mode)
  ;; This is not a real package so don't load it
  :defer t)

;; Doesn't work for helm commands
;; (read-string "Year (default 2019): " nil nil "2019")
(use-package minibuf-eldef ; Only show defaults in prompts when applicable
  :defer 1.2               ; To save 0.136sec
  :init
  ;; Must be set before minibuf-eldef is loaded
  (setq minibuffer-eldef-shorten-default t)
  :config
  (minibuffer-electric-default-mode))

(use-package chunyang-edit-minibuffer
  :bind (:map minibuffer-local-map ("C-c '" . chunyang-edit-minibuffer)))

(use-package which-key
  :disabled
  :ensure t
  :diminish which-key-mode
  :config (which-key-mode))


;;; Ivy

;; counsel -> swiper -> ivy
(use-package counsel
  :disabled
  ;; TODO Toggle between normal Emacs completing-read & Ivy
  ;; :disabled
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-do-completion-in-region nil)
  (setq ivy-count-format "")
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)      ; recentf in `ivy-switch-buffer'
  (setq counsel-find-file-at-point t)
  ;; TODO: Replace global-set-key with bind-key
  (global-set-key (kbd "M-i") 'swiper)
  (global-set-key (kbd "C-o") 'counsel-imenu)
  (global-set-key (kbd "M-I") 'counsel-ag)
  (global-set-key (kbd "C-z") 'ivy-resume)
  (global-set-key (kbd "M-l") 'ivy-switch-buffer)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (global-set-key (kbd "C-c f l") 'counsel-find-library)
  (global-set-key (kbd "C-h S") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package prescient
  :disabled
  :ensure t
  :homepage https://github.com/raxod502/prescient.el
  :about Simple but effective sorting and filtering
  :config (prescient-persist-mode))

(use-package ivy-prescient
  :ensure t
  :after ivy
  :config (ivy-prescient-mode))

(use-package company-prescient
  :disabled
  :ensure t
  :after company
  :config (company-prescient-mode))


;;; Helm

(use-package helm
  :ensure t
  :defer t
  :init
  ;; Disable Helm's C-x c bindings
  (setq helm-command-prefix-key nil)
  :bind (("M-l" . helm-mini)
         ("M-L" . helm-browse-project)
         ("M-i" . helm-occur)
         ("M-I" . helm-do-grep-ag)
         ("C-o" . helm-imenu)
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-z" . helm-resume)
         ("C-h a" . helm-apropos)
         ("C-c f l" . helm-locate-library))
  :diminish helm-mode
  :config
  (helm-mode)
  ;; Persist recent used of filtered candidates
  (helm-adaptive-mode)

  (setq helm-window-prefer-horizontal-split 'decide)

  (define-minor-mode chunyang-helm-window-hack-mode
    "Hack helm window display."
    :global t
    (let ((action '("\\`\\*helm"
                    (display-buffer-in-side-window)
                    (window-height . 0.4))))
      (if chunyang-helm-window-hack-mode
          (progn
            (add-to-list 'display-buffer-alist action)
            (setq helm-display-function #'display-buffer))
        (setq display-buffer-alist
              (delete action display-buffer-alist))
        (let ((standard-value (eval (car (get 'helm-display-function 'standard-value)))))
          (setq helm-display-function standard-value)))))

  ;; (chunyang-helm-window-hack-mode)

  (setq helm-grep-ag-command
        "rg --color=always --smart-case --no-heading --line-number %s %s %s")

  (with-eval-after-load 'helm-unicode
    (define-advice helm-unicode-insert-char (:override (_candidate) candidates)
      (insert
       (mapconcat
        (lambda (x) (substring x -1))
        (helm-marked-candidates)
        " ")))))

(use-package helm-ring
  :defer t
  :config
  (defun chunyang-helm-kill-ring-diff (_str)
    (let ((marked (helm-marked-candidates)))
      (unless (= (length marked) 2)
        (user-error "Need exact 2 marked candidates, got %d" (length marked)))
      (apply #'chunyang-ediff-strings-wordwise marked )))
  (add-to-list 'helm-kill-ring-actions
               (cons "Ediff" #'chunyang-helm-kill-ring-diff)
               t))

(use-package wgrep-helm
  :ensure t
  :defer t)

(use-package helm-ls-git
  :ensure t
  :config
  (setq helm-ls-git-default-sources
        '(helm-source-ls-git-buffers
          helm-source-ls-git))
  :defer t)

(use-package helm-fd
  :about find files using fd <https://github.com/sharkdp/fd> with helm
  :commands helm-fd)


;;; Buffers, Windows and Frames

(use-package uniquify  ; Make buffer names unique (turn on by default)
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package autorevert         ; Auto-revert buffers of changed files
  :diminish auto-revert-mode
  :config (global-auto-revert-mode))

(use-package simple
  :no-require t
  :config
  (define-advice goto-line (:before (&rest _) preview-line-number)
    "Preview line number when prompting for line number.
Idea from URL `https://www.reddit.com/r/emacs/comments/as83e2/weekly_tipstricketc_thread/egu2sve'."
    (interactive
     (lambda (spec)
       (if (and (boundp 'display-line-numbers) ; `display-line-numbers' was added in Emacs 26.1
                (not display-line-numbers))
           (unwind-protect
               (progn (display-line-numbers-mode)
                      (advice-eval-interactive-spec spec))
             (display-line-numbers-mode -1))
         (advice-eval-interactive-spec spec))))))

(use-package chunyang-simple
  :preface
  ;; 我已经用了用了 M-RET，结果 Org/Eww/Gnus 也用
  ;; 一个处理按键冲突的简单解决方法
  (defun chunyang-escape (key)
    "Execute KEY as it is in `global-map'."
    ;; NOTE M-RET not working in `markdown-mode'
    ;; (interactive "kExecute Key in global-map: ")
    (interactive (list (vector (read-key "Execute Key in global-map: "))))
    (call-interactively (lookup-key (current-global-map) key))
    (message nil))
  (defun chunyang-dispatch-key (key)
    (interactive "kDispatch Key: ")
    (require 'seq)
    (let* ((commands (seq-filter #'commandp
                                 (mapcar (lambda (map) (lookup-key map key))
                                         (current-active-maps))))
           (collection (mapcar #'symbol-name commands))
           (command (intern (completing-read
                             (concat (key-description key) " ")
                             collection nil t nil nil (cadr collection)))))
      (command-execute command)))
  :bind (("C-x 3" . chunyang-split-window-right)
         ("C-x 2" . chunyang-split-window-below)
         ("C-h t" . chunyang-switch-scratch)
         ;; C-\ runs `toggle-input-method' by default
         ;; ("C-\\"  . chunyang-dispatch-key)
         :map lisp-interaction-mode-map
         ("C-c C-l" . chunyang-scratch-clear)
         :map messages-buffer-mode-map
         ("C-c C-l" . chunyang-clear-messages-buffer))
  :commands (chunyang-window-click-swap
             chunyang-cycle-filename-format
             chunyang-display-number-as-char
             chunyang-delete-region
             chunyang-center-current-line
             chunyang-count-top-level-expression))

(use-package chunyang-misc
  :commands (chunyang-open-another-emacs
             chunyang-starify-region
             chunyang-timer
             chunyang-random-word
             chunyang-format-as-binary
             chunyang-toggle-proxy
             ;; Clipboard
             chunyang-insert-image-from-clipboard
             ;; QR Code
             chunyang-scan-qr-code-from-screen
             chunyang-qrdecode
             chunyang-qrencode
             ;; 迅雷
             chunyang-decode-thunder-link
             chunyang-encode-thunder-link
             ;; https://www.quotes.net
             chunyang-random-quote
             ;; HTML -> Markdown
             chunyang-yank-html
             ;; Eval
             chunyang-eval-in-other-emacs
             ;; Insert datetime
             chunyang-helm-time
             ;; Emacs -Q with straight.el
             chunyang-straight-emacs-Q-command
             chunyang-straight-git-version))

(use-package chunyang-buffers
  :commands chunyang-kill-buffer-query-function
  :init (add-hook 'kill-buffer-query-functions #'chunyang-kill-buffer-query-function)
  :preface
  (defun chunyang-kill-all-buffers (&optional except-current-buffer)
    "Kill all buffers.
With prefix argument, don't kill the current buffer."
    (interactive "P")
    (let ((buffers-to-kill
           (seq-filter
            (lambda (buffer)
              (let ((name (buffer-name buffer)))
                ;; Ignore uninteresting buffers
                (and (not (string-prefix-p " " name))
                     (not (member name '("*scratch*" "*Messages*")))
                     (not (and except-current-buffer (eq buffer (current-buffer)))))))
            (buffer-list))))
      (mapc #'kill-buffer buffers-to-kill)
      (delete-other-windows)))

  (defun chunyang-kill-dir-buffers (dir)
    "Kill all buffers whose `default-directory' is under DIR."
    (interactive (list default-directory))
    (setq dir (expand-file-name dir))
    (let ((count 0))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (and (string-prefix-p dir (expand-file-name default-directory))
               (kill-buffer buffer)
               (cl-incf count))))
      (message "Killed %d buffer%s in %s"
               count
               (if (> count 1) "s" "")
               (abbreviate-file-name dir))))

  (defun chunyang-kill-invisible-buffers ()
    "Kill all invisible buffers."
    (interactive)
    (seq-do
     #'kill-buffer
     (seq-difference
      (seq-filter
       (lambda (buffer)
         (not (string-prefix-p " " (buffer-name buffer))))
       (buffer-list))
      (seq-map #'window-buffer (window-list)))))
  :defer t
  :bind ("C-x K" . chunyang-kill-dir-buffers))

(bind-key "O"     #'delete-other-windows special-mode-map)
(bind-key "Q"     #'kill-this-buffer     special-mode-map)
(bind-key "C-x k" #'kill-this-buffer)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)           ; was `list-buffers'
  :config
  (define-ibuffer-column human-readable-size
    (
     :name "Size"
     :inline t
     :header-mouse-map ibuffer-size-header-map
     :summarizer
     (lambda (column-strings)
       (cl-loop for s in column-strings
                sum (get-text-property (1- (length s)) 'size s) into total
                finally return (file-size-human-readable total))))
    (let ((size (buffer-size)))
      (propertize (file-size-human-readable size)
                  'size size)))

  (setq ibuffer-formats
        '((mark " " (name 18 18 :left :elide)
                " " (human-readable-size 6 -1 :right)
                " " (mode 16 16 :left :elide) " " filename-and-process)
          (mark " " (name 16 -1) " " filename)))

  ;; Hide uninteresting and Helm buffers, use C-u g to toggle the display of
  ;; them
  (setq ibuffer-maybe-show-predicates
        (list
         (lambda (buf)
           (string-match "^ \\|\\*[Hh]elm" (buffer-name buf)))))
  
  ;; I used to M-o for other-window
  (unbind-key "M-o" ibuffer-mode-map))

(use-package ace-window
  :ensure t
  :defer t
  :preface
  (defun chunyang-ace-window (arg)
    "A modified version of `ace-window'.
When number of window <= 3, invoke `other-window', otherwise `ace-window'.
One C-u, swap window, two C-u, `chunyang-window-click-swap'."
    (interactive "p")
    (cl-case arg
      (0
       (setq aw-ignore-on
             (not aw-ignore-on))
       (ace-select-window))
      (4 (ace-swap-window))
      (16 (call-interactively #'chunyang-window-click-swap))
      (t (if (<= (length (window-list)) 3)
             (other-window 1)
           (ace-select-window)))))
  :bind ("M-o" . chunyang-ace-window)
  :config
  (setq aw-ignore-current t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package winner
  :init (setq winner-dont-bind-my-keys t)
  :config
  ;; New user option in Emacs 27.1
  (when (boundp 'winner-boring-buffers-regexp)
    (setq winner-boring-buffers-regexp "\\*[hH]elm.*"))
  (bind-keys :map winner-mode-map
             ("M-N" . winner-redo)
             ("M-P" . winner-undo))
  (winner-mode))

(use-package eyebrowse
  :disabled t
  :homepage https://github.com/wasamasa/eyebrowse
  :ensure t
  :config (eyebrowse-mode))

(use-package shackle
  :homepage https://github.com/wasamasa/shackle
  :ensure t
  :disabled t
  :commands shackle-mode
  :config (shackle-mode))

(use-package buffer-expose
  :ensure t
  :about Visual buffer switching using a window grid
  :homepage https://github.com/clemera/buffer-expose
  :defer t)

;; Frames
(setq frame-resize-pixelwise t)         ; Resize by pixels
(setq frame-title-format '(buffer-file-name "%f" "%b"))

(bind-key "M-RET" #'toggle-frame-maximized)


;;; File handle
;; Keep backup and auto save files out of the way
;; NOTE: Commenting this out because `no-littering.el' is doing this for me
;; (setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backup")))
;;       auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; For Cocoa Emacs.app, trashing just means move file to
;; ~/.local/share/Trash/, which is not very useful, so don't enable
;; this feature in such case.
(unless (and *is-mac* (not *is-mac-port*))
  ;; Delete files to trash
  ;; (setq delete-by-moving-to-trash t)
  ;; To support 'Put back' (but this is slow)
  ;; (setq mac-system-move-file-to-trash-use-finder t)
  )

;; Exit Emacs without confirm kill processes (Emacs-26.1)
(setq confirm-kill-processes nil)

;; .#foo.txt
;; 不同进程修改同一个文件
(setq create-lockfiles nil)
;; foo.txt~
;; 备份最近一个版本
(setq make-backup-files nil)
;; #foo.txt#
;; 定期预存，否则停电、系统崩溃等会损失没保存数据
(setq auto-save-default nil)

;; XXX 自动保存到硬盘，试用一段时间 2019-11-23
;; XXX 这是哪个版本引入的？看看能不能利用 package-lint 的函数
;; (auto-save-visited-mode)

(use-package files
  :bind (("C-c f u" . revert-buffer)
         ("C-c f n" . normal-mode))
  :preface
  (defun chunyang-add-file-local-variable-post (&rest _r)
    "Ask to save and revert the buffer."
    (when (y-or-n-p "Save and revert this file?")
      (save-buffer)
      (revert-buffer nil :no-confirm)))
  (advice-add 'add-file-local-variable :after #'chunyang-add-file-local-variable-post)
  (advice-add 'add-file-local-variable-prop-line :after #'chunyang-add-file-local-variable-post))

(use-package dired                      ; Directory Editor
  :defer t
  :preface
  (defun dired-finder-reveal-marked-files (files)
    "Reveal marked FILES in Finder.
FILES are a list of absolute filename.
FILES are in the same directory."
    (interactive (list (dired-get-marked-files)))
    (when files
      (do-applescript
       (format
        (concat "tell application \"Finder\"\n"
                "  activate\n"
                "  tell (make new Finder window) to set target to POSIX file \"%s\"\n"
                "  set selection to {%s}\n"
                "end tell")
        (file-name-directory (car files))
        (mapconcat
         (lambda (file)
           (format "POSIX file \"%s\"" file))
         files
         ", ")))))
  (defun chunyang-dired-view-file-other-window ()
    (interactive)
    (cl-letf (((symbol-function 'view-file) #'view-file-other-window))
      (dired-view-file)))
  :config
  (bind-key "V" #'chunyang-dired-view-file-other-window dired-mode-map)

  ;; It's better to use ls(1) from GNU Coreutils since it supports
  ;; --dired, thus Dired doesn't have to search filename. See
  ;; `insert-directory-program'.

  (and *is-mac*
       (executable-find "gls")
       (setq insert-directory-program "gls"))
  (setq dired-listing-switches "-Alh")

  ;; `dired-listing-switches' can't contain whitespace before
  ;; the commit c71b718be86bdda7b51c8ea0da30aa896a7833fe
  ;;
  ;; (when (version<= "26.0.50.2" emacs-version)
  ;;   (setq dired-listing-switches
  ;;         (combine-and-quote-strings '("-Alh" "--time-style=+%_m月 %d %H:%M"))))
  ;;
  ;; FIXME
  ;; 1. 这回导致 Tramp 的 Dired 不能正常工作
  ;; 2. Mode line 有点问题

  ;; Use directory from other dired window as target directory while
  ;; copying and renaming.
  (setq dired-dwim-target t)

  (when *is-mac*
    (defun chunyang-dired-reveal-file-in-Finder ()
      (interactive)
      (let ((file (dired-get-file-for-visit)))
        (chunyang-mac-Finder-reveal file)))

    (defun chunyang-dired-quick-look-file ()
      (interactive)
      (let ((file (dired-get-file-for-visit)))
        (shell-command (format "qlmanage -p '%s' &> /dev/null" file))))

    ;; It is very cool but can't compare with Finder, anyway,
    ;; qlmanage(1) is just for debugging.
    ;; (bind-key "SPC" #'chunyang-dired-quick-look-file dired-mode-map)
    ))

;; (info "(dired-x) Features")
(use-package dired-x
  ;; Note that dired-x also sets the following binding when it gets
  ;; loaded by default.
  :bind (("C-x C-j"   . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :init (eval-after-load 'dired '(require 'dired-x))
  :config

  ;; On macOS, use open(1) as guess shell command for some files
  (when *is-mac*
    (setq dired-guess-shell-alist-user
          (list
           (list (rx "."
                     (or
                      ;; Videos
                      "mp4" "avi" "mkv" "rmvb"
                      ;; Torrent
                      "torrent"
                      ;; PDF
                      "pdf"
                      ;; HTML
                      "html"
                      ;; Image
                      "gif" "png" "jpg" "jpeg")
                     string-end)
                 "open"))))

  (add-to-list 'dired-guess-shell-alist-user
               `("\\.el\\'"
                 "emacs -Q --batch -f batch-byte-compile"
                 ,(format "%s -Q --batch -f batch-byte-compile"
                          (shell-quote-argument
                           (concat invocation-directory invocation-name)))))
  (add-to-list 'dired-guess-shell-alist-user '("\\.py\\'" "python3")))

(use-package dired-du
  :notes du can be slow
  :ensure t
  :commands dired-du-mode
  :config (setq dired-du-size-format t))

(use-package async
  :ensure t
  :commands dired-async-mode)

(use-package direx                      ; Alternative to Dired
  :about Simple Directory Explorer
  :homepage https://github.com/m2ym/direx-el
  :ensure t
  :defer t)

(use-package ztree
  :about Text mode directory tree
  :homepage https://github.com/fourier/ztree
  :ensure t
  :defer t)

(use-package treemacs
  :about A tree style file explorer package
  :homepage https://github.com/Alexander-Miller/treemacs
  :ensure t
  :defer t)

(use-package launch                     ; Open files in external programs
  :disabled t
  :ensure t
  :defer t)

(use-package dired-sidebar      ; Show dired in tree using side window
  :homepage https://github.com/jojojames/dired-sidebar
  :ensure t
  :defer t)


;;; Basic Editing

;; Disable tabs, but given them proper width
(setq-default indent-tabs-mode nil
              tab-width 8)

(setq kill-ring-max 200                 ; More killed items
      ;; Save the contents of the clipboard to kill ring before killing
      save-interprogram-paste-before-kill t)

(use-package delsel
  :disabled
  :config (delete-selection-mode))

(use-package electric              ; Electric code layout
  :disabled t                      ; `electric-layout-mode' can be very annoying
  :config (electric-layout-mode))

(use-package elec-pair                  ; Electric pairs
  :config (electric-pair-mode))

;; Configure a reasonable fill column, indicate it in the buffer and
;; enable automatic filling
(setq-default fill-column 80)
;; or change it interactively via C-x f (`set-fill-column')

;; I prefer indent long-line code myself
;; (setq comment-auto-fill-only-comments t)

;; (add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)

;; (diminish 'auto-fill-function)          ; Not `auto-fill-mode' as usual

;; To change `fill-prefix' interactively, type C-x . (`set-fill-prefix')

;; (global-visual-line-mode)

(use-package visual-line-mode
  :defer t
  :no-require t
  :init
  ;; NOTE: `visual-line-mode' 或者说 `word-wrap' 不能处理中文
  ;; https://emacs-china.org/t/topic/2616
  ;;
  ;; 但是没了 `word-wrap'，也就没太多理由再用 `visual-line-mode'（？）
  ;; 因为 Emacs 默认本来就开启了 Line Wrap
  ;;
  ;; (defun chunyang-disable-word-wrap ()
  ;;   (setq word-wrap nil))
  ;; (add-hook 'visual-line-mode-hook #'chunyang-disable-word-wrap)

  ;; (define-minor-mode chinese-visual-line-mode
  ;;   "Like Visual Line mode excepting turning off `word-wrap'."
  ;;   :lighter ""
  ;;   (if chinese-visual-line-mode
  ;;       (progn (visual-line-mode)
  ;;              (setq word-wrap nil))
  ;;     (visual-line-mode -1)))
  )

(use-package visual-fill-column         ; `fill-column' for `visual-line-mode'
  :disabled t
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
    (bind-key "C-'" #'avy-isearch isearch-mode-map)))

(use-package pin :disabled t)

(use-package ace-link
  :ensure t
  :config (ace-link-setup-default))

(use-package zop-to-char                ; alternative to `zap-to-char'
  :ensure t
  ;; TODO: Make a lighter version (I just want to move the point)
  ;; :bind ("M-z" . zop-to-char)
  :defer t)

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

(use-package mark-align                 ; Align visualized (via Marking)
  :commands mark-align-mode)

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
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package canonicalize
  :disabled
  :about "Change github into GitHub"
  :bind ("M-c" . canonicalize-dwim))


;;; Whitespace - Highlight and Manage Whitespaces

(use-package whitespace               ; Highlight bad whitespace (tab)
  :diminish " Whitespace"
  ;; TODO: Consider turn on this mode by default
  :bind ("C-c t w" . whitespace-mode)
  :config
  ;; Specify which kind of blank is visualized
  (setq whitespace-style
        '(face
          ;; trailing blanks
          trailing
          ;; empty lines at beginning and/or end of buffer
          ;; empty
          ;; line is longer `whitespace-line-column'
          lines-tail
          ;; tab or space at the beginning of the line according to
          ;; `indent-tabs-mode'
          indentation
          ;; show tab as » (see `whitespace-display-mappings')
          tab-mark))
  ;; Use `fill-column'
  (setq whitespace-line-column nil))

;; Useful commands to manage whitespace, tab, newline:
;; `whitespace-cleanup'
;; `delete-trailing-whitespace'
;; `just-one-space'
;; `cycle-spacing'
;; `delete-blank-lines'


;;; Enable & Disable some commands

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'view-hello-file
     'disabled "I mistype C-h h a lot and it is too slow to block Emacs")

(put 'list-timers 'disabled nil)


;;; Navigation and scrolling

(setq scroll-preserve-screen-position 'always)

(setq scroll-error-top-bottom t         ; Move to beg/end of buffer before
                                        ; signalling an error
      ;; scroll-conservatively 10          ; Smooth Scrolling
      scroll-conservatively 0           ; NOTE Forzen Emacs during I-Search
      )

;; This option was added in Emacs 25.1. I've not noticed the speed improvement,
;; but let me try this for a while, refer to (info "(emacs) Scrolling")
;; and
;; https://emacs.stackexchange.com/questions/31402/how-to-avoid-scrolling-with-large-files-hanging-for-short-periods-of-time-hold
(setq fast-but-imprecise-scrolling t)

;; These settings make trackpad scrolling on OS X much more predictable
;; and smooth
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1))

;; (bind-key* "C-M-p" #'scroll-up-line)
;; (bind-key* "C-M-n" #'scroll-down-line)
;; Use `C-M-l' instead of twice `C-l' for a better view

(use-package page-break-lines           ; Turn page breaks into lines
  :disabled
  :ensure t
  :diminish page-break-lines-mode
  :defer t
  :preface
  (defun chunyang-add-hooks (hooks funcs &optional append local)
    (dolist (hook hooks)
      (dolist (func funcs)
        (add-hook hook func append local))))

  (defun chunyang-remove-hooks (hooks funcs &optional local)
    (dolist (hook hooks)
      (dolist (func funcs)
        (remove-hook hook func local))))
  :init
  (chunyang-add-hooks
   '(prog-mode-hook compilation-mode-hook outline-mode-hook help-mode-hook)
   '(page-break-lines-mode)))

(use-package outline                    ; Navigate outlines in buffers
  ;; TODO: Read (info "(emacs) Outline Mode") to learn this mode
  ;; if it is useful, consider making helm support.
  :disabled t
  :diminish outline-minor-mode
  :config (dolist (hook '(text-mode-hook prog-mode-hook))
            (add-hook hook #'outline-minor-mode)))

(use-package imenu
  :defer t
  :config)


;;; Search
;; | Command     | Note                            |
;; |-------------+---------------------------------|
;; | grep        | grep                            |
;; | lgrep       | grep + shell pattern for files  |
;; | find-grep   | find -exec grep                 |
;; | rgrep       | like find-grep but filter files |
;; | zgrep       | zgrep (search compressed file)  |
;; | vc-git-grep | git-grep                        |
;;
;; See also (info "(emacs) Grep Searching")

(use-package grep
  :defer t
  :config
  (and *is-mac*
       (executable-find "gfind")
       (setq find-program "gfind")))

(use-package wgrep :ensure t :defer t)

;; Notes that isearch is not a package and it is loaded from the very
;; beginning
(use-package isearch
  :no-require t
  :defer t
  :preface
  (setq isearch-allow-scroll t)

  (defun chunyang-isearch-mode-setup ()
    "If the region is on, use it as initial search string.
Intended to be added to `isearch-mode-hook'."
    ;; Note that the text of the region can be an invalid regexp
    (when (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (deactivate-mark)
        (goto-char beg)
        (isearch-yank-internal (lambda () end)))))

  (add-hook 'isearch-mode-hook #'chunyang-isearch-mode-setup)

  :config
  (when (version>= emacs-version "27")
    (setq isearch-lazy-count t)
    ;; (setq lazy-highlight-buffer t
    ;;       lazy-highlight-cleanup nil)
    (setq isearch-allow-scroll 'unlimited)))

(use-package re-builder
  :defer t
  :config
  ;; Escape 的工作就交给 Emacs 了
  (setq reb-re-syntax 'string))

(use-package xr
  :about Convert string regexp to rx notation
  :ensure t
  :defer t)

(use-package anzu                       ; Position/matches count for isearch
  :disabled t
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :config
  :config
  (setq anzu-replace-to-string-separator " => ")
  (bind-key "M-%" #'anzu-query-replace)
  (bind-key "C-M-%" #'anzu-query-replace-regexp))

(use-package plur
  :ensure t
  :bind ("C-c M-%" . plur-query-replace)
  :config (bind-key "M-{" #'plur-isearch-query-replace isearch-mode-map))

(use-package region-state
  :ensure t
  :load-path "~/src/region-state.el"
  :commands region-state-mode
  :init (region-state-mode))

(use-package swap-regions
  :about "利用 Recursive Edit 交换两个 Region"
  :load-path "~/src/swap-regions.el"
  :ensure t
  :bind ("C-c C-t" . swap-regions))

(use-package clear-text
  :disabled t
  :ensure t
  :commands (clear-text-mode global-clear-text-mode))

(use-package pinyin-search
  :ensure t
  :defer t)


;;; Highlight

;; Disable annoying mouse highlighting for Org Agenda, also see
;; https://emacs.stackexchange.com/questions/20651/turn-off-mouse-highlighting-in-org-agenda
(setq mouse-highlight 1)

(use-package hl-line
  :bind ("C-c t L" . hl-line-mode))

(use-package paren                      ; Highlight paired delimiters
  :config (show-paren-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package hl-todo
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'yaml-mode-hook #'hl-todo-mode)
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
  ;; https://github.com/emacs-mirror/emacs/blob/e243da257d/etc/NEWS.26#L931
  :unless (version< "26" emacs-version)
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
  :disabled t
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
  :defer t
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  ;; Remove some unused backends
  (setq-default company-backends
                '(company-capf
                  company-files
                  (company-dabbrev-code company-keywords)
                  company-dabbrev))
  ;; Use Company for completion C-M-i
  ;; (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  ;; M-h/c-h/F1 to display doc in help buffer, C-w to show location
  ;; (bind-key "M-h" #'company-show-doc-buffer company-active-map)
  (setq company-tooltip-align-annotations t
        company-minimum-prefix-length 2)
  ;; remove annoying blinking
  (setq company-echo-delay 0))

(use-package company-clang
  :defer t
  :config
  ;; XXX: candidates menu will prepend an leading space for each
  (define-advice company-clang--annotation (:filter-return (ann) one-space)
    "Prefer foo (arg) over foo(arg) in GNU coding style."
    (if (and ann (equal c-indentation-style "gnu"))
        (concat " " ann)
      ann)))

(use-package yasnippet
  :homepage http://joaotavora.github.io/yasnippet/
  :ensure t
  :diminish yas-minor-mode
  :init (setq yas-alias-to-yas/prefix-p nil)
  :config
  ;; (yas-global-mode)
  )

(use-package yasnippet-snippets
  :homepage https://github.com/AndreaCrotti/yasnippet-snippets
  :disabled
  :ensure t
  :after yasnippet
  :config (yas-reload-all))


;;; Spelling and syntax checking

(use-package flymake-diagnostic-at-point
  :homepage https://github.com/meqif/flymake-diagnostic-at-point
  :notes
  Other options
  - mouse hover
  - M-x display-local-help
  - flymake-show-diagnostics-buffer
  :ensure t
  :after flymake
  ;; 跟 eldoc 有冲突
  ;; :init (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  :config
  (setq
   ;; no prefix
   flymake-diagnostic-at-point-error-prefix nil

   ;; longer dealy, the same as `flycheck-display-errors-delay'
   flymake-diagnostic-at-point-timer-delay 0.9

   ;; prefer `message' over popup
   flymake-diagnostic-at-point-display-diagnostic-function
   'flymake-diagnostic-at-point-display-minibuffer))

(use-package flyspell
  :init
  ;; (add-hook 'text-mode-hook #'flyspell-mode)
  ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; Magit commit message
  (add-hook 'git-commit-mode-hook #'flyspell-mode)
  ;; VC commit message
  (add-hook 'log-edit-mode-hook #'flyspell-mode)
  ;; Email
  (add-hook 'message-mode-hook #'flyspell-mode)
  (use-package ispell
    :defer t
    :init
    (setq ispell-program-name "aspell"
          ispell-extra-args
          '("--sug-mode=fast"
            ;; NOTE 我把 macOS 改成了中文语言，然后 Emacs:
            ;;
            ;; (getenv "LANG")
            ;; ;; => "zh-Hans_US.UTF-8"
            ;;
            ;; 导致 Aspell 试图找 zh 语言的字典，遂失败
            "--lang=en_US")))
  :bind ("C-c t s" . flyspell-mode)
  :config
  (unbind-key "C-."   flyspell-mode-map)
  (unbind-key "C-M-i" flyspell-mode-map)
  (unbind-key "C-;"   flyspell-mode-map)
  (use-package flyspell-popup
    :ensure t
    :config
    (bind-key "C-." #'flyspell-popup-correct flyspell-mode-map)
    (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode)))


(use-package langtool       ; LanguageTool <https://languagetool.org/>
  :homepage https://github.com/mhayashi1120/Emacs-langtool
  :ensure t
  :defer t
  :config
  (setq langtool-language-tool-jar
        ;; Find newest version
        (car
         (sort
          (file-expand-wildcards
           "~/src/LanguageTool-*/languagetool-commandline.jar")
          #'string>))))

(use-package flycheck-languagetool
  :after flycheck
  :config (flycheck-languagetool-setup))

(use-package checkdoc
  :defer t
  ;; use `:init' instead of `:config' is because flycheck needs them
  ;; eagerly
  :init
  ;; 为了方便提交 Melpa，不修改默认设置
  '(setq
    checkdoc-force-docstrings-flag nil
    ;; This is the default since Emacs-26.1
    checkdoc-arguments-in-order-flag nil))

(use-package flycheck
  :ensure t
  :bind (("C-c t f" . flycheck-mode)
         ("C-c t F" . global-flycheck-mode))
  :config
  ;; * Emacs Lisp
  ;; I don't use `package.el' at all
  (setq-default flycheck-emacs-lisp-initialize-packages nil)
  ;; XXX Better idea? Such as figure just enough `load-path', such as
  ;; via `straight.el', `magit-emacs-Q-command'
  ;; 1) Figure out dependencies (recursively?)
  ;; 2) Find out load-path
  (setq-default flycheck-emacs-lisp-load-path 'inherit)

  (define-advice flycheck-may-enable-mode (:filter-return (enable-p) blacklist)
    (and enable-p
         ;; init.el
         (not (eq (current-buffer) (get-file-buffer user-init-file)))
         ;; `lisp-interaction-mode'
         (not (eq major-mode 'lisp-interaction-mode))
         ;; *scratch*
         (not (string= (buffer-name) "*scratch*"))
         ;; Emacs source code
         (not (and (buffer-file-name)
                   (string-prefix-p (expand-file-name source-directory)
                                    (buffer-file-name)))))))

(use-package flycheck-pos-tip       ; Show Flycheck messages in popups
  :disabled t
  :ensure t
  :after flycheck
  :config (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flycheck-color-mode-line
  :disabled t
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-color-mode-line-mode))

(use-package flycheck-package           ; Extra Emacs Lisp checker
  :ensure t
  :after flycheck
  :config
  (flycheck-package-setup)
  ;; Cause I don't use `package.el' anymore
  ;; (advice-add 'package-lint--check-packages-installable :around #'ignore)
  )

(use-package elsa
  :about Emacs Lisp Static Analyzer
  :homepage https://github.com/emacs-elsa/Elsa
  ;; :load-path "~/src/Elsa"
  :ensure t
  :defer t
  :preface
  (defun chunyang-elsa-batch-command ()
    (interactive)
    ;; To check FILENAME, use '$ elsa FILENAME', and elsa needs Elsafile.el to
    ;; work
    (let ((cmd (mapconcat
                #'shell-quote-argument
                `(,(expand-file-name "bin/elsa" (file-name-directory (locate-library "elsa")))
                  ,@(mapcan (lambda (lib)
                              (list "-L" (file-name-directory (locate-library lib))))
                            '("elsa" "dash" "f" "s" "trinary")))
                " ")))
      (message "Elsa batch command saved to kill-ring")
      (kill-new cmd))))

(use-package flycheck-elsa-without-cask
  :after flycheck)

(use-package flycheck-elsa
  :disabled
  :ensure t
  :after flycheck
  :config (flycheck-elsa-setup))


;;; Markup languages

;; TODO: Fontify markdown link like org, see `org-descriptive-links',
;; `orglink', and (elisp) Font Lock Mode.
(use-package markdown-mode
  :ensure t
  :defer t
  :preface
  (defun chunyang-markdown-link (url)
    (interactive (list (read-string "URL: " (thing-at-point 'url))))
    (let (title markdown)
      (with-current-buffer (url-retrieve-synchronously url)
        (set-buffer-multibyte t)
        (let ((dom (libxml-parse-html-region url-http-end-of-headers (point-max))))
          (require 'dom)
          (setq title (dom-text (car (dom-by-tag dom 'title))))))
      (setq markdown (format "[%s](%s)" title url))
      (kill-new markdown)
      (message "Copied: %s" markdown)
      markdown))
  (defun chunyang-markdown-insert-link (title link)
    (interactive
     (let ((title (read-string "Title: "))
           (link  (read-string "Link: ")))
       (list title link)))
    (insert (format "[%s](%s)" title link)))

  :mode ("README\\.md\\'" . gfm-mode)
  :config
  ;; `emacs-lisp', `elisp' and `el' are all Emacs Lisp
  (cl-pushnew '("el" . emacs-lisp-mode) markdown-code-lang-modes)
  (cl-pushnew '("emacs-lisp" . emacs-lisp-mode) markdown-code-lang-modes)

  (setq markdown-fontify-code-blocks-natively t)
  ;; Use M-x `markdown-edit-code-block' to edit code in another buffer

  (setq markdown-command "pandoc -s -f markdown -t html")

  ;; Live Preview in Chrome
  (defun chunyang-markdown-preview-in-chrome ()
    "Export Markdown and preview the result in Chrome.
This function reuses the current tab of Chrome,
unlike `markdown-preview'."
    (interactive)
    (let ((output (markdown-export)))
      (if (string-match-p (regexp-quote output) (chunyang-chrome-url))
          (chunyang-chrome-refresh)
        (browse-url output))))

  (unbind-key "M-RET" markdown-mode-map)

  (define-minor-mode chunyang-markdown-preview-in-chrome-mode
    "Run `chunyang-markdown-preview-in-chrome' on save."
    :lighter " MD-Preview-in-Chrome"
    (unless (eq major-mode 'markdown-mode)
      (user-error "Error: %s is not Markdown Mode" major-mode))
    (if chunyang-markdown-preview-in-chrome-mode
        (add-hook 'after-save-hook #'chunyang-markdown-preview-in-chrome :append :local)
      (remove-hook 'after-save-hook #'chunyang-markdown-preview-in-chrome :local))))

(use-package mdcx
  :load-path "~/src/mdcx.el"
  :about "Run Markdown Code block"
  :after markdown-mode
  :config
  (bind-key "C-c C-c" #'mdcx markdown-mode-map)

  (defun mdcx-run-racket (src)
    ;; TODO: support image, check sandbox
    ;; https://github.com/Bogdanp/try-racket/blob/755d18aa3052d0a3658f74131d06b4d9356b3a6e/try-racket/components/playground.rkt
    ;; https://en.wikipedia.org/wiki/Racket_features#Resource_Management_and_Sandboxing
    ;;
    ;; (mdcx-markdown-code-block-lang-info)
    (shell-command-to-string
     (concat "racket " (make-temp-file nil nil ".rkt" src)))))

(use-package edit-indirect
  :about "Edit Markdown code block like Org"
  :ensure t
  :defer t)

(use-package bbcode-mode
  :homepage https://en.wikipedia.org/wiki/BBCode
  :about Lightweight Markup Language used by phpBB forumss
  :ensure t
  :defer t)

(use-package mediawiki
  :homepage https://www.mediawiki.org/wiki/MediaWiki
  :ensure t
  :commands mediawiki-mode)

(use-package deft
  :homepage https://jblevins.org/projects/deft
  :about "quickly browse, filter, and edit plain text notes"
  :ensure t
  :commands deft
  :init
  ;; it has to been set before deft.el is loaded otherwise
  ;; `deft-default-extension' will not use it
  (setq deft-extensions '("md" "org"))
  :config
  (setq deft-directory "~/Notes")
  (setq deft-markdown-mode-title-level 1))

(use-package dnote
  :about "Manage Dnote notes in Emacs" https://dnote.io/
  :commands (dnote-add dnote-find))

(use-package chunyang-temp-buffer
  :commands chunyang-temp-buffer)


;;; PDF

(use-package pdf-tools
  :disabled
  :homepage https://github.com/politza/pdf-tools
  :ensure t
  ;; FIXME I do not know how to setup pdf-tools
  :init
  (pdf-loader-install)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;    恢复页面                                           ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; 持久化？
  (defvar chunyang-pdf-alist nil
    "List of (FILENAME . PAGE).")

  (defun chunyang-pdf-view-mode-save-page ()
    (when (and (eq major-mode 'pdf-view-mode)
               buffer-file-name)
      (setq chunyang-pdf-alist
            (delq (assoc buffer-file-name chunyang-pdf-alist)
                  chunyang-pdf-alist))
      (push (cons buffer-file-name (pdf-view-current-page))
            chunyang-pdf-alist)))

  (add-hook 'kill-buffer-hook #'chunyang-pdf-view-mode-save-page 'append)

  (defun chunyang-pdf-view-mode-open-page ()
    (when-let ((page (assoc-default buffer-file-name chunyang-pdf-alist)))
      (pdf-view-goto-page page)))

  (add-hook 'pdf-view-mode-hook #'chunyang-pdf-view-mode-open-page 'append)

  "the ugly placement (for better line-base diff)")


;;; Programming utilities

;; `glasses-mode' -- 把 areYouReady 显示成 are_You_Ready
;; `subword-mode' -- 把 StudlyCapsIdentifiers 当作一个 word
;; `superword-mode' -- 把 this_is_a_symbol 当作一个 word
;; `hs-minor-mode' -- Hideshow / 折叠、隐藏

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :config
  ;; Alternative to `eldoc-minibuffer-message'
  (defun chunyang-eldoc-header-line-message (format-string &rest args)
    (setq header-line-format
          (apply #'format format-string args))
    (force-mode-line-update))

  ;; (setq eldoc-message-function #'chunyang-eldoc-header-line-message)
  )

(declare-function projectile-project-root "projectile")
(defun chunyang-project-root ()
  "Return project root. If no project is found, return nil."
  (cond ((require 'project nil t) (cdr (project-current)))
        ((require 'projectile nil t) (ignore-errors (projectile-project-root)))
        (t (error
            "`project.el' or `projectile.el' is required to locate project root"))))

(use-package compile
  :bind (("C-x c" . compile))
  :preface

  (defun chunyang-compile-command ()
    "Guess a `compile-command' for the current buffer."
    (when-let ((file (and buffer-file-name (file-name-nondirectory buffer-file-name)))
               (command (pcase major-mode
                          ('c-mode "cc")
                          ('elixir-mode "elixir")
                          ((guard (derived-mode-p 'emacs-lisp-mode))
                           "emacs -Q --batch -f batch-byte-compile"))))
      (format "%s %s" command (shell-quote-argument file))))

  (defun chunyang-compile-command-set ()
    (pcase (chunyang-compile-command)
      ((and command (guard command))
       (setq-local compile-command command))))

  (add-hook 'prog-mode-hook #'chunyang-compile-command-set)

  ;; (defvar chunyang-compilation-root nil)
  ;; (defun chunyang-compilation-setup ()
  ;;   (setq chunyang-compilation-root (chunyang-project-root)))
  ;; (defun chunyang-compilation-save-buffers-predicate ()
  ;;   (file-in-directory-p (buffer-file-name) chunyang-compilation-root))

  :config

  ;; FIXME: This is not working on 25.1.1 from macOS
  ;; Only ask for saving files under current project
  ;; (setq compilation-process-setup-function 'chunyang-compilation-setup)
  ;; (setq compilation-save-buffers-predicate
  ;;       'chunyang-compilation-save-buffers-predicate)

  (setq compilation-always-kill t
        compilation-scroll-output 'first-error)

  ;; Don't ask for save, just save for me, ti's for convenience though not safe
  (setq compilation-ask-about-save nil))

(use-package quickrun
  :homepage https://github.com/syohex/emacs-quickrun
  :ensure t
  :defer t)

(use-package prog-mode
  :bind ("C-c t p" . prettify-symbols-mode)
  ;; TODO: I have some font issue, so disalbe it for now
  ;; :init (add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
  :init (global-prettify-symbols-mode))

(use-package nocomments-mode            ; Hide Comments
  :ensure t
  :defer t)

(use-package rebox2
  :ensure t
  :defer t)

(use-package chunyang-comment
  :commands (chunyang-comment-section
             chunyang-insert-comment-section))

(use-package keyfreq
  :disabled
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))


;; Languages

(use-package json-mode
  :about Prefer json-mode to js-mode
  :homepage https://github.com/joshwnj/json-mode
  :ensure t
  :defer t)

(use-package yaml-mode :ensure t :defer t)


;;; Generic Lisp

(use-package paredit                    ; Balanced sexp editing
  :ensure t
  :diminish paredit-mode
  :commands paredit-mode
  :init
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
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

(use-package smartparens
  :disabled
  :homepage  https://github.com/Fuco1/smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))


;;; Emacs Lisp

(use-package elisp-mode
  :defer t
  :preface
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

  (let ((sym-regexp (or (bound-and-true-p lisp-mode-symbol-regexp)
                        "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")))
    (add-to-list
     'lisp-imenu-generic-expression
     (list "Packages"
           (concat "^\\s-*("
                   (eval-when-compile
                     (regexp-opt '("use-package" "require") t))
                   "\\s-+\\(" sym-regexp "\\)")
           2))
    (add-to-list
     'lisp-imenu-generic-expression
     (list "Hydras"
           (concat "^\\s-*("
                   (eval-when-compile
                     (regexp-opt '("defhydra") t))
                   "\\s-+\\(" sym-regexp "\\)")
           2)))

  (define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
    "If SYM is a function, append its docstring."
    (concat
     (apply orig-fun sym r)
     (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
            (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
       (and oneline
            (stringp oneline)
            (not (string= "" oneline))
            (concat "  |  " (propertize oneline 'face 'italic))))))

  (define-advice eval-print-last-sexp (:around (old-fun &rest args) add-prefix)
    "Prepend ;; =>."
    (let ((op (point)))
      (apply old-fun args)
      (save-excursion
        (goto-char op)
        (forward-line 1)
        (insert ";; => "))))

  (define-advice elisp--preceding-sexp (:around (old-fun) multiline-comment)
    "Support sexp in multiline comment."
    (condition-case err
        (funcall old-fun)
      (scan-error
       (if (nth 4 (syntax-ppss))
           (let ((work-buffer (current-buffer))
                 (temp-buffer (generate-new-buffer " *temp*"))
                 found sexp error)
             (with-current-buffer temp-buffer
               (delay-mode-hooks (emacs-lisp-mode)))
             (save-excursion
               (comment-normalize-vars)
               (while (and (comment-beginning)
                           (not found))
                 (let ((code (buffer-substring-no-properties
                              (point) (line-end-position))))
                   (with-current-buffer temp-buffer
                     (goto-char (point-min))
                     (insert code ?\n)
                     (goto-char (point-max))
                     (condition-case err
                         (setq sexp (funcall old-fun)
                               found t)
                       (scan-error (setq error err)))))
                 (when (= -1 (forward-line -1))
                   (error "elisp--preceding-sexp@multiline-comment error"))
                 (goto-char (line-end-position))))
             (cond (found sexp)
                   (error (signal (car error) (cdr error)))
                   (t (error "elisp--preceding-sexp@multiline-comment error"))))
         (signal (car err) (cdr err)))))))

(use-package aggressive-indent
  :disabled t
  :ensure t
  :defer t
  :diminish aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  :config
  (push 'chunyang-eval-print-last-sexp
        aggressive-indent-protected-commands)
  (push 'chunyang-macroexpand-print-last-sexp
        aggressive-indent-protected-commands))

(use-package el-search
  :disabled
  :ensure t
  :defer t
  :preface
  (defun chunyang-el-search-symbol-or-sexp-at-point ()
    ;; Adopted from `el-search-this-sexp'
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((symbol-at-point-text (thing-at-point 'symbol))
            symbol-at-point)
        (if (and symbol-at-point-text
                 ;; That should ideally be always true but isn't
                 (condition-case nil
                     (symbolp (setq symbol-at-point (read symbol-at-point-text)))
                   (invalid-read-syntax nil)))
            `',symbol-at-point
          (when (thing-at-point 'sexp)
            `',(sexp-at-point))))))
  (defun chunyang-el-search-git-repo (directory pattern)
    "El-search all elisp files in current Git repository."
    (interactive
     (progn
       (require 'el-search)
       (require 'magit)
       (let* ((directory
               (or (magit-toplevel)
                   ;; Limited to `magit-repository-directories'
                   (magit-read-repository)))
              (default (format "%s" (chunyang-el-search-symbol-or-sexp-at-point)))
              (input
               (el-search--read-pattern
                (format
                 "El-search %s for pattern: "
                 (abbreviate-file-name directory))
                default))
              (pattern (read input)))
         (list directory pattern))))
    (el-search-setup-search
     pattern
     (lambda ()
       (stream
        (seq-filter (lambda (s) (string-suffix-p ".el" s))
                    (let ((default-directory directory))
                      (mapcar #'expand-file-name (magit-tracked-files))))))
     (lambda (search)
       (setf (alist-get 'description (el-search-object-properties search))
             (concat "Search the git repo in " (abbreviate-file-name directory))))))

  (defun chunyang-el-search-package (pattern package)
    "Search pattern in directory of PACKAGE (a symbol) recursively.
PACKAGE should not be a built-in package."
    (interactive
     (list (el-search-read-pattern-for-interactive
            "Search package for pattern: ")
           (intern
            (completing-read
             "el-search package: "
             (mapcar 'car package-alist) nil t))))
    (let ((pkgdir
           (package-desc-dir
            (cadr (assq package package-alist)))))
      (el-search-directory pattern pkgdir t)))

  (defun chunyang-el-search-elpa (pattern)
    (interactive
     (let* ((default (format "%s" (chunyang-el-search-symbol-or-sexp-at-point)))
            (input
             (el-search--read-pattern
              (format
               "El-search %s for pattern: "
               (abbreviate-file-name package-user-dir))
              default))
            (pattern (read input)))
       (list pattern)))
    (el-search-directory pattern package-user-dir t))

  :init
  ;; Reduce startup time. keys won't work untill an elisp file is opened.
  ;;
  ;; UPDATE: Well, it dosen't really matter, since `elisp-mode' is always loaded
  ;; even in Emacs -Q.
  (with-eval-after-load 'elisp-mode
    (el-search-install-shift-bindings))
  :config
  (defun el-search-helm ()
    (interactive)
    (el-search--message-no-log "Preparing helm...")
    (unless el-search--current-search
      (user-error "No active search"))
    (let* ((search el-search--current-search)
           (stream-of-matches (funcall (el-search-object-get-matches search)))
           (candidates (mapcar
                        (pcase-lambda ((and match `(,buffer ,match-beg ,file)))
                          (with-current-buffer buffer
                            (cons (buffer-substring-no-properties
                                   match-beg (el-search--end-of-sexp match-beg))
                                  match)))
                        (seq-into stream-of-matches 'list))))
      (require 'helm)
      (helm :sources
            (helm-build-sync-source
                (el-search--get-search-description-string el-search--current-search)
              :candidates candidates
              :multiline t
              :action (pcase-lambda (`(,buffer ,match-beg ,file))
                        (if file
                            (find-file file)
                          (switch-to-buffer buffer))
                        (goto-char match-beg)))
            :buffer "*helm el-search*")))

  ;; XXX: Better idea for the buffer name, see C-j C-S-j
  ;; `el-search-jump-to-search-head'
  (defun chunyang-el-search-change-occur-buffer-name ()
    (when el-search-occur-search-object
      (setf (buffer-name)
            (generate-new-buffer-name
             (format "*El Occur: %s*"
                     (cl-substitute
                      ?\s ?\n
                      (prin1-to-string (el-search-object-pattern
                                        el-search-occur-search-object))
                      :test #'=))))))
  (add-hook 'el-search-occur-mode-hook #'chunyang-el-search-change-occur-buffer-name)
  (require 'el-search-x)

  ;; FIXME: Update this list every time el-search starts. How?
  (defvar el-search--symbols-with-doc-string
    (let (symbols)
      (mapatoms
       (lambda (sym)
         (and (fboundp sym)
              (get sym 'doc-string-elt)
              (push sym symbols))))
      symbols)
    "A list of symbols which support doc-string.")

  (defun el-search--doc-string-p ()
    "Return t if point is at docstring."
    (pcase (save-excursion
             (backward-up-list)
             (read (current-buffer)))
      (`(,(and symbol (guard (memq symbol el-search--symbols-with-doc-string)))
         ,_ . ,_)
       (let ((op (point)))
         (save-excursion
           (backward-up-list)
           (forward-char)
           (ignore-errors
             (forward-sexp (1+ (get symbol 'doc-string-elt)))
             (backward-sexp)
             (= op (point))))))))

  ;; Type C-h f `el-search-defined-patterns' to learn defined patterns
  (el-search-defpattern doc-string (&rest regexps)
    "Match any documentation string that is matched by all REGEXPS."
    `(and (string ,@regexps) (guard (el-search--doc-string-p))))

  (el-search-defpattern s (&rest regexps)
    "Match any string (excluding doc string) that is matched by all REGEXPS"
    `(and (string ,@regexps) (guard (not (el-search--doc-string-p))))))

(use-package lispy
  :disabled
  :ensure t
  :homepage https://github.com/abo-abo/lispy
  :commands (lispy-alt-multiline        ; Format lisp code
             lispy-oneline))

(use-package emr
  :disabled
  :about "Emacs Refactor (EMR) is a framework for providing
  language-specific refactoring in Emacs. It includes refactoring
  commands for a variety of languages, including elisp itself!"
  :homepage https://github.com/Wilfred/emacs-refactor
  :ensure t
  :commands emr-show-refactor-menu)

(use-package chunyang-elisp
  :commands (chunyang-format-command-on-key
             chunyang-format-help-on-key
             chunyang-eval-last-sexp-in-next-window
             chunyang-eval-print-last-sexp
             chunyang-macroexpand-print-last-sexp
             chunyang-display-mark-and-pos-mode
             chunyang-display-window-mode
             display-command-mode
             chunyang-toggle-setq-form
             chunyang-insert-command
             chunyang-insert-key)
  :preface
  (defun threadify (exp)
    (cl-labels ((aux (exp acc)
                     (pcase exp
                       (`(,func ,arg1)
                        (aux arg1 (cons func acc)))
                       (`(,func ,arg1 . ,args)
                        (aux arg1 (cons (cons func args) acc)))
                       (X `(-> ,X ,@acc)))))
      (aux exp ())))

  (defun threadify-sexp-at-point ()
    (interactive)
    (unless (sexp-at-point)
      (user-error "No sexp at point"))
    (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'sexp)))
      (let ((new (pp-to-string (threadify (sexp-at-point)))))
        (delete-region beg end)
        (insert new)))))

(use-package chunyang-package
  :commands (describe-package--add-melpa-link
             chunyang-package-menu-hack-tabulated-list-format)
  :init
  (when (>= emacs-major-version 25)
    (advice-add 'describe-package-1 :after #'describe-package--add-melpa-link))

  (add-hook 'package-menu-mode-hook #'chunyang-package-menu-hack-tabulated-list-format t))

(use-package ielm
  :defer t
  :config
  (add-hook 'ielm-mode-hook #'enable-paredit-mode))

(use-package macrostep
  :ensure t
  ;; should only for elisp
  :bind ("C-c e" . macrostep-expand))

(use-package litable
  :homepage https://github.com/Fuco1/litable
  :about On-the-fly evaluation/substitution of emacs lisp code
  :ensure t
  :defer t)

(use-package pcache              :ensure t :defer t)
(use-package persistent-soft     :ensure t :defer t)
(use-package persist             :ensure t :defer t)
(use-package log4e               :ensure t :defer t)
(use-package alert               :ensure t :defer t)

(use-package bug-hunter                 ; This is good
  :disabled t
  :ensure t :defer t)

(use-package debbugs                    ; Interface to GNU Bugs
  :ensure t
  :defer t
  :preface
  ;; https://debbugs.gnu.org/cgi/pkgreport.cgi?which=submitter&data=mail%40xuchunyang.me
  (defun chunyang-list-emacs-bugs ()
    "List emacs bugs submitted by me."
    (interactive)
    (let ((debbugs-gnu-current-query `((submitter . ,user-mail-address))))
      (debbugs-gnu nil)))

  ;; TODO: Fontify #1234 (make it clickable) in certain modes
  (defun chunyang-open-emacs-bug (id)
    "Open emacs bug report in browser, the bug id looks like Bug#25942."
    (interactive
     (list
      (or
       (let ((case-fold-search t))
         (when (thing-at-point-looking-at (rx (opt "Bug#") (group (1+ num))) 10)
           (string-to-number (match-string 1))))
       (read-number "Bug Id: "))))
    (let ((url
           (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                   id)))
      (message "Opening %s ..." (propertize url 'face 'link))
      (browse-url url))))

(use-package hydra
  :homepage https://github.com/abo-abo/hydra
  :ensure t
  :defer t)

(use-package debug
  :defer t
  :preface
  ;; (DEBUG emacs-version (+ 1 2 3))
  (defmacro DEBUG (&rest exprs)
    `(message
      ,(concat "[DEBUG] "
               (mapconcat
                (lambda (e)
                  (concat
                   (replace-regexp-in-string
                    "%" "%%" (prin1-to-string e))
                   " = %S"))
                exprs ", "))
      ,@exprs))
  :config
  (defhydra hydra-debugger-menu ()
    "Debug"
    ("c" debugger-continue "Continue")
    ("e" debugger-eval-expression "Eval")
    ("v" debugger-toggle-locals "Display local Variable"))

  (bind-key "." #'hydra-debugger-menu/body debugger-mode-map))

(use-package edebug
  :info (info "(elisp) Edebug")
  :defer t
  :config

  (define-advice edebug-eval-expression (:before (_expr) better-interactive-form)
    "Fix the original interactive form."
    (interactive (list (read--expression "Edebug Eval: "))))

  ;; Don't pause after every break (Added in Emacs 26.1)
  (setq edebug-sit-on-break nil)
  ;; Don't restore window configuration
  ;; - For safety, it should be t, but it usually is pointless & slow & annoying
  ;; - Use W to toggle this option
  (setq edebug-save-windows nil))

(use-package nameless
  :ensure t
  :defer t)

(use-package package-lint               ; Check Compatibility
  :ensure t
  :defer t)

(use-package ert
  :about The built-in library for write tests for Emacs Lisp
  :info (info "(ert) Top")
  :defer t)

(use-package buttercup
  :about "Behavior-Driven Emacs Lisp Testing, alternative to `ert'"
  :ensure t
  :homepage https://github.com/jorgenschaefer/emacs-buttercup/
  :defer t)

(use-package assess
  :about "Additional support for testing Emacs Lisp"
  :ensure t
  :homepage https://github.com/phillord/assess
  :defer t)

(use-package testcover
  :about Visual code-coverage tool (based on Edebug)
  :info (info "(elisp) Test Coverage")
  :defer t)

(use-package undercover
  :about base on Edebug and generate output for Coveralls
  :homepage https://github.com/sviridov/undercover.el
  :ensure t
  :defer t)

(use-package helpful
  :ensure t
  :defer t)

(use-package ghelp
  :disabled
  :homepage https://github.com/casouri/ghelp
  :defer t
  :load-path "~/src/ghelp")

(use-package elisp-demos
  :load-path "~/src/elisp-demos"
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package other-emacs-eval
  :ensure t
  :defer t)

(use-package with-emacs
  :ensure t
  :homepage https://github.com/twlz0ne/with-emacs.el
  :defer t)

(use-package cask
  :about Project management for Emacs package development
  :homepage http://github.com/cask/cask
  :disabled t)

(use-package cask-mode
  :about Major mode for editing Cask files
  :ensure t
  :defer t)

(use-package emake
  :about Test Elisp without the hoops
  :homepage https://github.com/vermiculus/emake.el
  :disabled t)

(use-package font-lock-studio
  :about Debugger for Font Lock keywords
  :homepage https://github.com/Lindydancer/font-lock-studio
  :ensure t
  :commands font-lock-studio)

(use-package auto-compile
  :about No more outdated *.elc
  :ensure t
  :commands auto-compile-mode
  :init (add-hook 'emacs-lisp-mode-hook #'auto-compile-mode))


;;; Help & Info

(use-package find-func
  :bind ("C-h C-k" . find-function-on-key))

(use-package find-key-binding
  :load-path "~/src/find-key-binding.el"
  :bind ("C-h C-b" . find-key-binding))

(use-package help
  :preface
  (defun view-help-buffer ()
    "View the `*Help*' buffer."
    (interactive)
    (pop-to-buffer (help-buffer)))
  (defun help-info-lookup-symbol ()
    (interactive)
    (when-let ((symbol (cadr help-xref-stack-item)))
      (info-lookup-symbol symbol)))
  (defun chunyang-describe-symbol-at-point ()
    "Like `describe-symbol' but doesn't query always."
    (interactive)
    (require 'help-mode)
    (describe-symbol
     (or (pcase (variable-at-point)
           (0 nil)
           (v v))
         (function-called-at-point)
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
           sym))))

  (defun chunyang-advice-remove-button (function)
    "Add a button to remove advice."
    (when (get-buffer "*Help*")
      (with-current-buffer "*Help*"
        (save-excursion
          (goto-char (point-min))
          ;; Before Emacs 27
          ;; :around advice: ‘shell-command--shell-command-with-editor-mode’
          ;; Since Emacs 27
          ;; This function has :around advice: `shell-command--shell-command-with-editor-mode'.
          (while (re-search-forward " advice: [‘'`]\\(.+\\)[’'']" nil t)
            (let ((advice (intern-soft (match-string 1))))
              (when (and advice (fboundp advice))
                (let ((inhibit-read-only t))
                  (insert " » ")
                  (insert-text-button
                   "Remove"
                   'action
                   ;; In case lexical-binding is off
                   `(lambda (_)
                      (message "Removing %s of advice from %s" ',function ',advice)
                      (advice-remove ',function #',advice)
                      (revert-buffer nil t))
                   'follow-link t)))))))))
  :bind (("C-h ." . chunyang-describe-symbol-at-point)
         ;; On vanilla Emacs, C-h . runs `display-local-help', which shows
         ;; help-echo at point, so it provides an keyboard alternative to muse
         ;; hover
         ("C-h h" . view-help-buffer)
         :map help-mode-map
         ("b" . help-go-back)
         ("f" . help-go-forward)
         ("i" . help-info-lookup-symbol))
  :init
  ;; Disable aggressive auto loading during C-h f/v (Emacs 26.3)
  (setq help-enable-completion-auto-load nil)
  :config
  (temp-buffer-resize-mode)
  (advice-add 'describe-function-1 :after #'chunyang-advice-remove-button))

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
               ("(magit) Variable Index"           nil "^ -+ .*: " "\\( \\|$\\)")
               ("(magit) Command Index"            nil "^ -+ .*: " "\\( \\|$\\)")
               ("(magit) Function Index"           nil nil nil)
               ;; Gnus
               ("(gnus) Index"                     nil "['`‘]" "['’]"))))

(use-package info
  :defer t
  :config
  ;; Install libc info manual on macOS
  ;;
  ;; $ cd ~/.emacs.d/var/info/
  ;; $ curl --proxy socks5://127.0.0.1:1080 -O https://www.gnu.org/software/libc/manual/info/libc-info.tar.gz
  ;; $ tar xvf libc-info.tar.gz
  ;; $ install-info libc.info dir
  (add-to-list 'Info-directory-list "~/.emacs.d/var/info")

  ;; Get HTML link
  ;; (emacs) Echo Area
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Echo-Area.html
  ;;
  ;; (elisp) The Echo Area
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html
  ;; TODO Support even more Info manuals
  ;; TODO Convert html url to info node
  (defvar chunyang-Info-html-alist
    '(
      ;; Special cases come first
      ("org"  . "http://orgmode.org/manual/%s")
      ("find" . "https://www.gnu.org/software/findutils/manual/html_node/find_html/%s")
      ("standards" . "https://www.gnu.org/prep/standards/html_node/%s")
      ("magit" . "https://magit.vc/manual/magit/%s")
      ("slime" . "https://www.common-lisp.net/project/slime/doc/html/%s")
      ;; Haha, just for some fun
      ("geiser" . (lambda (_)
                    ;; (info "(geiser) First aids")
                    ;; http://www.nongnu.org/geiser/geiser_3.html#First-aids
                    (if (string=  Info-current-node "Top")
                        "http://www.nongnu.org/geiser/"
                      (let ((node (replace-regexp-in-string " " "-" Info-current-node))
                            (number (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward "^[0-9]")
                                      (match-string 0))))

                        (format "http://www.nongnu.org/geiser/geiser_%s.html#%s" number node)))))
      ("gawk" . "https://www.gnu.org/software/tar/manual/html_node/%s")
      ;; GNU info documents.  Taken from my memory or see https://www.gnu.org/software/
      (("awk" "sed" "tar" "make" "m4" "grep" "coreutils" "guile" "screen"
        "libc" "make" "gzip" "diffutils" "wget" "grub") .
        (lambda (software)
          (format "https://www.gnu.org/software/%s/manual/html_node/%%s" software)))
      ("gcc-5" . "https://gcc.gnu.org/onlinedocs/gcc/%s")
      (("gdb" "stabs") .
       (lambda (software)
         (format "https://sourceware.org/gdb/onlinedocs/%s/%%s" software)))
      ;; Emacs info documents.  Taken from `org-info-emacs-documents'
      (("ada-mode" "auth" "autotype" "bovine" "calc" "ccmode" "cl" "dbus" "dired-x"
        "ebrowse" "ede" "ediff" "edt" "efaq-w32" "efaq" "eieio" "eintr" "elisp"
        "emacs-gnutls" "emacs-mime" "emacs" "epa" "erc" "ert" "eshell" "eudc" "eww"
        "flymake" "forms" "gnus" "htmlfontify" "idlwave" "ido" "info" "mairix-el"
        "message" "mh-e" "newsticker" "nxml-mode" "octave-mode" "org" "pcl-cvs"
        "pgg" "rcirc" "reftex" "remember" "sasl" "sc" "semantic" "ses" "sieve"
        "smtpmail" "speedbar" "srecode" "todo-mode" "tramp" "url" "vip" "viper"
        "widget" "wisent" "woman") .
        (lambda (package)
          (format "https://www.gnu.org/software/emacs/manual/html_node/%s/%%s" package)))))

  (defun chunyang-org-info-map-anchor-url (node)
    "Return URL associated to Info NODE."
    (require 'org)                      ; for `org-trim'
    ;; See (info "(texinfo) HTML Xref Node Name Expansion") for the
    ;; expansion rule
    (let* ((node (replace-regexp-in-string "[ \t\n\r]+" " " (org-trim node)))
           (node (mapconcat (lambda (c)
                              (if (string-match "[a-zA-Z0-9 ]" (string c))
                                  (string c)
                                (format "_%04x" c)))
                            (string-to-list node) ""))
           (node (replace-regexp-in-string " " "-" node))
           (url (if (string= node "")
                    ""
                  (if (string-match "[0-9]" (substring node 0 1))
                      (concat "g_t" node)
                    node))))
      url))
  (defun chunyang-Info-get-current-node-html ()
    (cl-assert (eq major-mode 'Info-mode))
    (let* ((file (file-name-nondirectory Info-current-file))
           (node Info-current-node)
           (html (if (string= node "Top")
                     ""
                   (concat (chunyang-org-info-map-anchor-url node) ".html")))
           (baseurl (cl-loop for (k . v) in chunyang-Info-html-alist
                             when (cond ((stringp k) (equal file k))
                                        ((listp k) (member file k)))
                             return (if (stringp v) v (funcall v file)))))
      ;; Maybe it's a good idea to assuming GNU softwares in this case
      (cl-assert baseurl nil "Unsupported info document '%s'" file)
      (format baseurl html)))

  ;; TODO: Consider using `defhydra' or `helm' for these actions
  ;; Copy: (elisp) Cons Cells
  ;; Copy: (info "(elisp) Cons Cells")
  ;; Copy: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cons-Cells.html
  ;; Open: https://www.gnu.org/software/emacs/manual/html_node/elisp/Cons-Cells.html
  ;; Copy: Markdown: ...
  ;; Copy: Org: ...

  (defun chunyang-Info-copy-current-node-html ()
    (interactive)
    (let ((url (chunyang-Info-get-current-node-html)))
      (kill-new url)
      (message "Copied: %s" url)))

  (defun chunyang-Info-browse-current-node-html ()
    (interactive)
    (let ((url (chunyang-Info-get-current-node-html)))
      (browse-url url)))

  (defun chunyang-Info-markdown-current-node-html (&optional arg)
    "ARG will be passed to `Info-copy-current-node-name'."
    (interactive "P")
    (let ((description (Info-copy-current-node-name arg))
          (link (chunyang-Info-get-current-node-html)))
      (let ((markdown (format "[%s](%s)" description link)))
        (kill-new markdown)
        (message "Copied: %s" markdown))))

  (defun chunyang-Info-org-current-node-html (&optional arg)
    "ARG will be passed to `Info-copy-current-node-name'."
    (interactive "P")
    (let ((description (Info-copy-current-node-name arg))
          (link (chunyang-Info-get-current-node-html)))
      (let ((org (format "[[%s][%s]]" link description)))
        (kill-new org)
        (message "Copied: %s" org))))

  (bind-key "C" #'chunyang-Info-copy-current-node-html Info-mode-map))

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


;;; Version Control

(use-package diff-mode
  :defer t
  :config
  ;; I used this key for M-o (`chunyang-ace-window')
  (unbind-key "M-o" diff-mode-map))

(use-package magit
  :ensure t
  :homepage https://github.com/magit/magit
  :info (info "(magit) Top")
  :bind (("C-x g"   . magit-status))
  :config
  (setq-default magit-diff-refine-hunk t)
  (setq magit-save-repository-buffers 'dontask)
  ;; M-x `magit-list-repositories'
  (setq magit-repository-directories
        '(("~/.emacs.d"                . 0)
          ("~/.emacs.d/straight/repos" . 1)
          ("~/src"                     . 1))))


(use-package ghub
  :ensure t
  :defer t
  :config
  ;; FIXME https://github.com/magit/ghub/issues/81
  (setq ghub-use-workaround-for-emacs-bug nil)
  :notes
  ;; Examples
  (ghub-get "/")
  (ghub-get "/user")
  (ghub-get "/user/starred"))

(use-package forge
  :homepage https://github.com/magit/forge
  :about Work with Git forges from the comfort of Magit
  :ensure t
  :defer t)

(use-package magithub
  :disabled
  :ensure t
  :after magit
  :config (magithub-feature-autoinject t))

(use-package vc
  :defer t
  :init
  ;; Disable VC entirely
  (setq vc-handled-backends ())
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
  :ensure t
  :defer t)

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
  :defer t
  :preface
  (defun helm-gitignore-template ()
    (interactive)
    (require 'helm)
    (helm :sources
          (helm-build-in-buffer-source "Gitignore Templates"
            :data
            (split-string
             (shell-command-to-string
              "curl -s https://api.github.com/gitignore/templates | jq -r '.[]'")
             "\n")
            :action
            (let ((new-action
                   (lambda (fun)
                     (lambda (lang)
                       (funcall
                        fun
                        (shell-command-to-string
                         (format
                          "curl -s https://api.github.com/gitignore/templates/%s | jq -r '.source'"
                          lang)))))))
              (helm-make-actions
               "Insert" (funcall new-action #'insert)
               "Copy" (funcall new-action #'kill-new))))
          :buffer "*helm-gitignore-templates*")))

(use-package chunyang-git
  :commands chunyang-git-tree)


;;; Tools and utilities

(use-package prodigy
  :about Manage external services
  :homepage https://github.com/rejeep/prodigy.el
  :ensure t
  :defer t
  :config
  (prodigy-define-service
    :name "Godoc Server"
    :command "godoc"
    :args '("-http=:8000")
    :url "http://localhost:8000"
    :cwd "~/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(use-package hyperbole
  :ensure t
  :homepage https://www.gnu.org/software/hyperbole/
  :notes
  - {M-x apropos RET hyperbole RET}
  - ~/.emacs.d/
  - https://example.com
  ;; - github#/rswgnu/hyperbole
  :init (setq hkey-init nil)
  :commands
  ;; M-RET, C-h A, C-h h
  (hkey-either hkey-help hyperbole))

(use-package repeater
  :about "C-n C-n C-n ... End of the buffer"
  :ensure t
  :defer t)

(use-package speeddating
  :about "Increasing and decreasing dates & time"
  :homepage https://github.com/xuchunyang/emacs-speeddating
  :ensure t
  :defer t)

(use-package woman
  :defer t
  :config (setq woman-fill-frame t))

(use-package comint
  :defer t
  :init
  (define-advice comint-run (:before (_program) fix-interactive-form)
    "Fix the original interactive form."
    (interactive (list (read-shell-command "Run program: "))))
  :config
  ;; Disable auto scroll on RET (like Eshell)
  (setq comint-scroll-show-maximum-output nil))

(use-package edit-chrome-textarea
  :load-path "~/src/edit-chrome-textarea.el"
  :commands edit-chrome-textarea
  :config
  (defun chunyang-edit-chrome-textarea-guess-mode (url title content)
    (pcase url
      ((rx (or "emacs-china.org"
               "github.com"
               "stackexchange.com"
               "stackoverflow.com"))
       (gfm-mode))
      (_
       (text-mode))))

  (setq edit-chrome-textarea-guess-mode-function
        #'chunyang-edit-chrome-textarea-guess-mode))

;; XXX: Not working under EXWM
(use-package atomic-chrome
  :disabled                             ; not used for a long time
  :ensure t                             ; To install its dependencies
  :defer 7                              ; since the entry of this
                                        ; package is from Chrome
  :preface
  (defun chunyang-atomic-chrome-server-running-p ()
    (cond ((executable-find "ss")       ; iproute2
           (zerop (call-process-shell-command "ss -tna | grep 64292")))
          ((executable-find "lsof")     ; lsof
           (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
          ((executable-find "netstat")  ; net-tools
           (zerop (call-process-shell-command "netstat -aon | grep 64292")))))
  :config
  (setq atomic-chrome-url-major-mode-alist
        '(
          ;; https://courses.edx.org/courses/course-v1:MITx+6.00.1x+2T2017_2/course/
          ("6\\.00\\.1" . python-mode)
          ;; [[https://guides.github.com/features/mastering-markdown/][Mastering Markdown · GitHub Guides]]
          ("github\\.com"             . gfm-mode)
          ;; [[https://stackoverflow.com/editing-help][Markdown Editing Help - Stack Overflow]]
          ("stackoverflow\\.com"      . markdown-mode)
          ("stackexchange\\.com"      . markdown-mode)
          ;; [[http://commonmark.org/help/][Markdown Reference]] (Discourse)
          ("emacs-china\\.org"        . markdown-mode)
          ("meta\\.discoursecn\\.org" . markdown-mode)
          ("0x00sec\\.org"            . markdown-mode)
          ("forums\\.debiancn\\.org"  . markdown-mode)
          ("users\\.rust-lang\\.org"  . markdown-mode)
          ("meta\\.appinn\\.com"      . markdown-mode)
          ;; [[https://en.wikipedia.org/wiki/Help:Wiki_markup][Help:Wiki markup - Wikipedia]] (MediaWiki)
          ("wiki\\.archlinux\\.org"   . mediawiki-mode)
          ;; [[https://en.wikipedia.org/wiki/BBCode][BBCode - Wikipedia]] (phpBB)
          ("bbs\\.archlinux\\.org"    . bbcode-mode)
          ("bbs\\.archlinuxcn\\.org"  . bbcode-mode)))

  (defun chunyang-atomic-chrome-mode-setup ()
    (setq header-line-format
          (substitute-command-keys
           "Edit Chrome text area.  Finish \
`\\[atomic-chrome-close-current-buffer]'.")))

  (add-hook 'atomic-chrome-edit-mode-hook #'chunyang-atomic-chrome-mode-setup)

  (if (chunyang-atomic-chrome-server-running-p)
      (message "Can't start atomic-chrome server, because port 64292 is already used")
    (atomic-chrome-start-server)))

(use-package diff
  :defer t
  :preface
  ;; http://mbork.pl/2019-11-17_Diffing_buffer_fragments%2c_continued
  (defun chunyang-diff-last-two-kills ()
    "Diff last two kills."
    (interactive)
    (cl-flet ((nb
               (name contents)
               (with-current-buffer (get-buffer-create name)
                 (insert contents)
                 (current-buffer))))
      (let ((new (nb " *diff: this-kill*" (current-kill 0 t)))
            (old (nb " *diff: last-kill*" (current-kill 1 t))))
        (diff old new)
        (kill-buffer new)
        (kill-buffer old)))))

(use-package ediff
  :defer t
  :preface
  (defun chunyang-dired-ediff (file-a file-b)
    (interactive
     (let ((files (dired-get-marked-files)))
       (if (= (length files) 2)
           (list (car files) (cadr files))
         (let ((file-a (dired-get-filename nil t)))
           (unless file-a
             (setq file-a (read-file-name "File A to compare: ")))
           (list file-a (read-file-name (format "Diff %s with: " file-a)))))))
    (ediff-files file-a file-b))

  (defun chunyang-ediff-last-two-kills ()
    "Ediff last two kills."
    (interactive)
    (chunyang-ediff-strings-wordwise (current-kill 0 t) (current-kill 1 t)))

  (defun chunyang-ediff-strings-wordwise (s-A s-B)
    (cl-flet ((nb
               (name contents)
               (with-current-buffer (get-buffer-create name)
                 (erase-buffer)
                 (insert contents)
                 (list (current-buffer) (point-min) (point-max)))))
      (pcase-let ((`(,buffer-A ,beg-A ,end-A) (nb " *ediff: A*" s-A))
                  (`(,buffer-B ,beg-B ,end-B) (nb " *ediff: B*" s-B)))
        ;; `ediff-regions-internal' is not autoloaded
        (require 'ediff)
        (ediff-regions-internal buffer-A beg-A end-A
                                buffer-B beg-B end-B
                                nil 'ediff-regions-wordwise 'word-mode nil))))
  :config
  ;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; or --unified, more compact context, see
  ;; (info "(diffutils) Unified Format")
  (setq ediff-custom-diff-options "-u")
  ;; Restore previous Window Configuration
  ;; https://emacs.stackexchange.com/questions/7482/restoring-windows-and-layout-after-an-ediff-session/7486
  ;; (setq ediff-quit-hook '(ediff-cleanup-mess winner-undo))
  (add-hook 'ediff-quit-hook #'winner-undo 'append))

(use-package server
  :defer 3
  :config
  (unless (server-running-p) (server-mode))

  (defun open-emacs-window ()
    (select-frame-set-input-focus (selected-frame)))

  (when *is-mac*
    ;; Re-focus iTerm
    (defun chunyang-activate-iterm ()
      ;; When committing with Magit, `server-done' is also called
      (unless (bound-and-true-p with-editor-mode)
        (do-applescript "tell application \"iTerm\" to activate")))
    (add-hook 'server-done-hook #'chunyang-activate-iterm)

    ;; Save temporary file without query
    (setq server-temp-file-regexp
          (rx string-start (eval temporary-file-directory))))

  (require 'org-protocol))

(use-package gh-md             :ensure t :defer t)

(use-package github-notifier
  :ensure t
  :defer t)

(use-package gitignore-templates
  :load-path "~/src/gitignore-templates.el"
  :ensure t
  :defer t)

(use-package github-stars
  :load-path "~/src/github-stars.el"
  :ensure t
  :defer t)

(use-package chunyang-github
  :ensure ghub                          ; Dependency
  :commands (helm-chunyang-github-stars
             helm-chunyang-github-repos)
  :preface
  (defun chunyang-github-create ()
    "Create a new repository on GitHub and add a git remote for it."
    (interactive)
    (require 'ghub)
    (let* ((git-root (or (locate-dominating-file default-directory ".git")
                         (user-error "Not a git repository")))
           (basename (file-name-nondirectory (directory-file-name git-root)))
           (params `((name . ,basename)
                     (private . ,(y-or-n-p "Private GitHub Repository?"))))
           (response (ghub-post "/user/repos" params)))
      (call-process-shell-command
       (concat "git remote add origin " (alist-get 'clone_url response)))
      (message "%s" (alist-get 'html_url response)))))

(use-package git-link
  :homepage https://github.com/sshaw/git-link
  :ensure t
  :defer t
  :config (setq git-link-open-in-browser t))

(use-package tramp                      ; Work with remote files
  ;; The entry point is find-file. Examples
  ;;
  ;; /sudo::/etc/shells
  ;; /ssh:xcy@xuchunyang.me:/home/xcy/  or just
  ;; /xuchunyang.me:/home/xcy
  ;; /ftp:hmwzynmu@6MK2JSCNAME.FYVPS.COM:/domains/foo.xuchunyang.me/public_html/
  ;;
  ;; Dired, Magit, Shell (M-x shell /ssh:xuchunyang.me:/bin/bash) and
  ;; Eshell works, while ansi-term doesn't not. See
  ;; (info "(tramp) Remote processes")
  ;;
  ;; To disable Tramp, set `tramp-mode' to nil.
  :defer t
  :config
  ;; Disable version control to avoid delays:
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; By default, Tramp re-computes directory every 10s.
  (setq tramp-completion-reread-directory-timeout nil))

(use-package sudo-edit
  :ensure t
  :defer t
  :notes
  ;; Tramp syntax with sudo
  - (find-file "/sudo::/etc/hosts")
  - (find-file "/ssh:xcy@arch|sudo:root@arch:/etc/hosts")
  :preface
  ;; Alternative to `sudo-edit', for the case that sudo is not possible
  ;;
  ;; XXX: Maybe support local as well, but now I don't need it, since I can
  ;; use sudo in all my local computers
  ;;
  ;; (find-file "/ssh:xcy@elpa:/etc/hosts")
  ;; ==>
  ;; (find-file "/ssh:root@elpa:/etc/hosts")
  (defun chunyang-edit-file-as-root ()
    "Edit the current remote file as root."
    (interactive)
    (let ((file buffer-file-name))
      (and file
           (file-remote-p file)
           (not (string= "root" (file-remote-p file 'user)))
           (find-alternate-file
            (tramp-make-tramp-file-name
             (file-remote-p file 'method)
             "root"
             (file-remote-p file 'host)
             (file-remote-p file 'localname))))))

  (defun chunyang-sudo-edit-notify ()
    "Notify myself when edit a file owned by root.
This should be add to `find-file-hook'."
    (let ((old-msg (current-message)))
      (when (and old-msg
                 (string= old-msg "Note: file is write protected")
                 ;; `chunyang-sudo-edit' doesn't work for remote files
                 ;; for now
                 (not (file-remote-p (buffer-file-name))))
        (message "%s, %s"
                 old-msg
                 "use M-x sudo-edit RET to edit via sudo"))))

  :init (add-hook 'find-file-hook #'chunyang-sudo-edit-notify))

(use-package ansible-doc
  :ensure t
  :commands ansible-doc)

(use-package helm-lastpass
  :ensure t
  :load-path "~/src/helm-lastpass"
  :commands helm-lastpass)

(use-package 1password
  :load-path "~/src/1password.el"
  :commands (1password-login 1password-get-password))

(use-package irfc
  :about Read RFC within Emacs
  :notes https://datatracker.ietf.org/
  :disabled t                           ; EmacsWiki
  :defer t
  :mode ("/rfc[0-9]+\\.txt\\'" . irfc-mode)
  :commands irfc-visit)

(use-package rfc-mode
  :ensure t
  :about Read and browse RFC documents
  :homepage https://github.com/galdor/rfc-mode
  :init (setq rfc-mode-directory (locate-user-emacs-file "var/rfc-mode/"))
  :commands (rfc-mode-browse rfc-mode-read))

(use-package symbolic-link-on-save
  :about Create Symbolic Link on save
  :commands symbolic-link-on-save-mode)

(use-package firestarter
  :about Execute (shell) commands on save
  :homepage https://github.com/wasamasa/firestarter
  :ensure t
  :defer t)

(use-package gif-screencast
  :about One-frame-per-action GIF recording for optimal quality/size ratio
  :homepage https://gitlab.com/ambrevar/emacs-gif-screencast
  :ensure t
  :defer t
  :config
  (when *is-mac*
    (setq gif-screencast-args '("-x"))
    (setq gif-screencast-capture-format "ppm"))

  (bind-key "C-c C-c" #'gif-screencast-stop gif-screencast-mode-map))

(use-package keycast
  :about Show current command and its key in the mode line
  :ensure t
  :homepage https://github.com/tarsius/keycast
  :defer t)

(use-package haikunator
  :commands haikunator-insert)

(use-package tinypng
  :homepage https://github.com/xuchunyang/tinypng.el
  :about compress .png and .jpg using TinyPNG.com
  :load-path "~/src/tinypng.el"
  :commands tinypng)

(use-package alfred
  :if *is-mac*
  :commands alfred-search)

(use-package dash-alfred
  :load-path "~/src/dash-alfred.el"
  :homepage https://github.com/xuchunyang/dash-alfred.el
  :commands dash-alfred-helm)

(use-package tongbu
  :load-path "~/src/tongbu.el"
  :commands tongbu)

(use-package separedit
  :about Edit comment/string/docstring/code block in separate buffer
  :homepage https://github.com/twlz0ne/separedit.el
  :ensure t
  :commands separedit)


;;; Documentation

(use-package dashdoc
  :load-path "~/src/DashDoc"
  :commands dashdoc
  :config
  (ivy-set-actions
   'dashdoc-ivy
   '(("b"
      (lambda (x)
        (browse-url (get-text-property 0 'quicklookurl x)))
      "browse url"))))

(use-package dash-at-point
  :ensure t
  :defer t)

(use-package zeal-at-point
  :ensure t
  :defer t)

(use-package chunyang-helm-SDL-api
  :about helm interface for https://wiki.libsdl.org/CategoryAPI
  :commands chunyang-helm-SDL-api)


;;; Project

(use-package project
  :defer t
  :preface
  ;; VCS are supported out-of-the-box but it doesn't work for me since I
  ;; disabled the vc entirely via `vc-handled-backends'.
  ;;
  ;; `project-try-vc'
  (defun chunyang-git-root (dir)
    (when-let ((root (locate-dominating-file dir ".git")))
      (cons 'git root)))
  :config
  ;; HOLD this, maybe it is better that Gopls works for a package per instance.
  ;; (add-hook 'project-find-functions #'chunyang-git-root)
  (cl-defmethod project-roots ((project (head git)))
    (list (cdr project))))

(use-package projectile
  :ensure t
  :defer t)


;;; Web & IRC & Email & RSS

(use-package rcirc
  :commands rcirc
  :config
  (setq rcirc-default-nick "chunyang")
  (setq rcirc-server-alist
        '(("irc.freenode.net" :channels ("#emacs"))
          ("irc.mozilla.org" :channels ("#rust"))))
  ;; Keep history.
  (setq rcirc-log-flag t)
  (setq rcirc-log-directory "~/.emacs.d/var/rcirc-log")
  ;; Taken from
  ;; https://github.com/s1n4/dotfiles/blob/master/emacs.d/config/rcirc-config.el
  (defun wh/log-filename-with-date (process target)
    (format
     "%s_%s.log"
     (if target
         (rcirc-generate-new-buffer-name process target)
       (process-name process))
     (format-time-string "%Y-%m-%d")))

  (setq rcirc-log-filename-function #'wh/log-filename-with-date)
  ;; Ignore away/join/part messages from lurkers.
  (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
  (add-hook 'rcirc-mode-hook #'rcirc-omit-mode)
  (use-package rcirc-color
    :after rcirc
    :ensure t))

(use-package erc
  :defer t
  :config
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT")))

;; Just by providing the following, Emacs can already send emails
;;
;; Password is provided in ~/.authinfo file (this file should be encrypted via gpg)
(setq user-full-name       "Xu Chunyang"
      user-mail-address    "xuchunyang56@gmail.com"
      ;; This is required for ~/.authinfo.gpg but not ~/.authinfo
      smtpmail-smtp-user   user-mail-address
      smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

;; Which MUA (Mail User Agent) or Email client to use in C-x m?
(setq mail-user-agent 'message-user-agent)
;; "--text follows this line--" is verbose
(setq mail-header-separator "")

(use-package message
  :defer t
  :functions epa-mail-default-recipients
  :notes
  ((message-elide-region         . "C-c C-e 省略一段冗长的引用")
   (message-mark-inserted-region . "C-c M-m 给一段文字（常常是代码）加上框"))
  :preface
  (defun chunyang-message-signature ()
    "Setup message signature adaptively."
    (require 'epa-mail)
    (let ((real-recipients (epa-mail-default-recipients)))
      (cond ((member "emacs-orgmode@gnu.org" real-recipients)
             (call-interactively #'org-version))
            ((member "emacs-devel@gnu.org" real-recipients)
             nil)
            (t nil))))
  :config
  (setq message-send-mail-function 'message-smtpmail-send-it)
  (setq message-default-headers
        (format "User-Agent: Emacs/%s"
                emacs-version))
  ;; don't keep message buffers around
  ;; (setq message-kill-buffer-on-exit t)
  (setq message-directory (locate-user-emacs-file "var/Mail"))
  (setq message-signature 'chunyang-message-signature))

(use-package gnus
  :defer t
  :config (setq gnus-select-method '(nnimap "imap.fastmail.com")))

(use-package notmuch
  :disabled
  :load-path "~/src/notmuch/emacs/"
  :commands notmuch
  :preface
  (defun chunyang-notmuch-update ()
    (interactive)
    (shell-command
     "notmuch tag -unread -inbox -- tag:unread \
and tag:inbox and from:mail@xuchunyang.me && \
proxychains4 mbsync --verbose --all && notmuch new&")
    (when-let ((buffer (get-buffer "*notmuch-hello*")))
      (with-current-buffer buffer
        (notmuch-refresh-this-buffer))))
  :config
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-logo nil)
  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
  (setq notmuch-fcc-dirs "Sent")
  (bind-key "g" 'notmuch-refresh-this-buffer notmuch-common-keymap))

(use-package ace-link-notmuch
  :after notmuch
  :config (ace-link-notmuch-setup))

(use-package helm-notmuch
  :disabled
  :ensure t
  :defer t)

(use-package mu4e
  :commands mu4e
  :config
  (add-to-list 'mu4e-view-actions
               '("open in web browser" .
                 chunyang-mu4e-action-open-html-in-browser))
  (defun chunyang-mu4e-action-open-html-in-browser (msg)
    (when-let* ((html (mu4e-message-field msg :body-html))
                (tmpfile (make-temp-file "mure-" nil ".html" html)))
      (call-process "open" nil nil nil tmpfile)))

  (setq mu4e-get-mail-command "offlineimap"))

(use-package chunyang-mail
  :commands chunyang-browse-gnu-message)

(use-package chunyang-display-mail-mode
  :about Display count of unread mail in the mode line
  :commands chunyang-display-mail-mode)

(use-package vdirel
  :about Access Contacts using vdirsyncer via the CardDAV protocol
  :homepage https://github.com/DamienCassou/vdirel
  :ensure t
  :config (setq vdirel-repository "~/.contacts/Default")
  :commands vdirel-helm-select-email)

(use-package sx
  :ensure t :defer t)

(use-package twittering-mode
  :ensure t
  :defer t
  :config
  (setq twittering-proxy-use t
        twittering-proxy-server "127.0.0.1"
        twittering-proxy-port 1087))

(use-package elfeed
  :ensure t
  :commands elfeed
  :config
  (setq elfeed-curl-extra-arguments '("--proxy" "socks5://127.0.0.1:1080"
                                      "--retry" "3"
                                      "--insecure"))
  (setq elfeed-curl-timeout 60)
  ;; TODO 用 Org Mode 管理 Feeds
  ;; - Elfeed
  ;; - OPML
  ;; - JSON
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          ("http://emacsninja.com/emacs.atom" emacs)
          ("http://emacshorrors.com/feed.atom" emacs)
          ("http://planet.emacsen.org/atom.xml" emacs)
          ("https://emacs-china.org/latest.rss" emacs)
          ("http://ubuntupodcast.org/feed/" ubuntu)
          "https://sspai.com/feed"
          "https://sirsharpest.github.io/rss.xml"
          "http://feeds2.feedburner.com/stevelosh"
          "http://feed.williamlong.info/"
          "http://matt.might.net/articles/feed.rss"
          "https://olivierpieters.be/feed.xml"
          "https://two-wrongs.com/feed.xml"
          "http://alex-charlton.com/rss.xml"
          "https://jameshfisher.com/feed.xml"
          "https://open.nytimes.com/feed"
          "http://bluxte.net/rss.xml"
          "https://increment.com/feed.xml")))

(use-package elfeed-org
  :disabled                             ; TODO 用 Org Mode 保存 Feed
                                        ; 很好，但我想用一个更轻量级的
                                        ; 方案，只要设置
                                        ; `elfeed-feeds' 就可以了
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/path/to/elfeed.org")))

(use-package elfeed-goodies
  :disabled
  :ensure t
  :config (elfeed-goodies/setup))

(use-package pocket-reader
  :homepage https://github.com/alphapapa/pocket-reader.el
  :about Client for https://getpocket.com
  :ensure t
  :defer t)

(use-package larder
  :load-path "~/src/larder.el"
  :homepage https://github.com/xuchunyang/larder.el
  :commands (larder-org
             larder-list-bookmarks
             larder-helm
             larder-add-bookmark
             larder-add-bookmark-widget))

(use-package url
  :defer t
  :config
  ;; XXX Don't ever save cookie to disk, since I don't know what's purpose of
  ;; cookie, and more importantly, keep loogging the following per hour is
  ;; annoying
  ;;
  ;; Saving file /Users/xcy/.emacs.d/var/url/configuration/cookies...
  ;; (advice-add 'url-cookie-setup-save-timer :override #'ignore)

  ;; Turn on cache, see https://emacs-china.org/t/url-el-cache/9411
  (setq url-automatic-caching t
        url-cache-expire-time (* 6 3600)))
(use-package curl-to-elisp
  :load-path "~/src/curl-to-elisp"
  :commands curl-to-elisp)

(use-package eww
  :defer t
  :preface
  (defun chunyang-eww-toggle-image ()
    (interactive)
    (setq shr-inhibit-images (not shr-inhibit-images))
    (eww-reload)
    (message "Image is now %s"
             (if shr-inhibit-images "off" "on")))

  (defun chunyang-eww-visit-chrome-tab ()
    "Visit URL of current Chrome Tab with EWW."
    (interactive)
    (unless  (eq system-type 'darwin)
      (user-error "Not supported system, only works on macOS"))
    (let ((url (do-applescript
                (concat "tell application \"Google Chrome\" to "
                        "return URL of active tab of first window"))))
      ;; Remove double quotes if exists
      (setq url (string-trim url "\"" "\""))
      (when (string-prefix-p "file://" url)
        (setq url (url-unhex-string url)
              url (replace-regexp-in-string "\\?.*$" "" url)))
      (eww url)))

  (defun helm-eww-bookmarks ()
    "My EWW bookmarks manager using helm."
    (interactive)
    (require 'helm)
    (require 'helm-adaptive)
    (require 'eww)
    (helm :sources
          (list
           (helm-build-sync-source "EWW Bookmarks"
             :candidates
             (lambda ()
               (cl-loop for elt in (eww-read-bookmarks)
                        collect
                        (cons (concat (plist-get elt :title)
                                      "\n"
                                      (propertize (plist-get elt :url)
                                                  'face 'link))
                              (plist-get elt :url))))
             :multiline t
             :action (helm-make-actions
                      "Eww" #'eww
                      "Browse-url" #'browse-url
                      "Copy URL" (lambda (url)
                                   (kill-new url)
                                   (message "Copied: %s" url)))
             :filtered-candidate-transformer #'helm-adaptive-sort)
           (helm-build-dummy-source "EWW URL"
             :action #'eww))
          :buffer "*Helm EWW Bookmarks*"))

  (defun chunyang-eww-import-bookmarks (bookmarks-html-file)
    "Import bookmarks from BOOKMARKS-HTML-FILE.
Note that this will OVERRIDE the existing EWW bookmarks."
    (interactive "fBookmarks HTML File: ")

    ;; Check if some libraries exist
    (require 'eww)
    (unless (require 'dom nil 'no-error)
      (user-error "dom.el not available, it was added in Emacs 25.1"))
    (unless (fboundp 'libxml-parse-html-region)
      (user-error "`libxml-parse-html-region' not available, \
your Emacs doesn't have libxml2 support"))

    (with-temp-buffer
      (insert-file-contents bookmarks-html-file)
      (setq eww-bookmarks
            (cl-loop with dom = (libxml-parse-html-region (point-min) (point-max))
                     for a in (dom-by-tag dom 'a)
                     for url = (dom-attr a 'href)
                     for title = (dom-text a)
                     for time = (current-time-string
                                 (seconds-to-time
                                  (string-to-number
                                   (dom-attr a 'add_date))))
                     collect (list :url url
                                   :title title
                                   :time time)))
      (eww-write-bookmarks)))

  (defun chunyang-eww-import-bookmarks-from-chrome-1 (json)
    "Adapted from `helm-chrome--add-bookmark'."
    (when (and (listp json) (listp (cdr json)))
      (cond
       ((assq 'roots json)
        (dolist (item (alist-get 'roots json))
          (chunyang-eww-import-bookmarks-from-chrome-1 item)))
       ((equal (alist-get 'type json) "folder")
        (cl-loop for item across (alist-get 'children json)
                 do (chunyang-eww-import-bookmarks-from-chrome-1 item)))
       ((equal (alist-get 'type json) "url")
        (setq eww-bookmarks
              (append eww-bookmarks
                      (list (list
                             :url  (alist-get 'url json)
                             :title (alist-get 'name json)
                             :time (seconds-to-time
                                    (string-to-number
                                     (let ((str (alist-get 'date_added json)))
                                       ;; 1 second = 100,000 micro seconds
                                       (substring str 0 (- (length str) 6)))))))))))))

  (defun chunyang-eww-import-bookmarks-from-chrome ()
    "Import bookmarks from Google Chrome's Bookmarks JSON file.
Note that this will OVERRIDE the existing EWW bookmarks."
    (interactive)
    (let ((bookmark-file
           (cl-find-if
            #'file-exists-p
            `("~/Library/Application Support/Google/Chrome/Profile 1/Bookmarks"
              "~/.config/chromium/Default/Bookmarks"))))
      (require 'json)
      (chunyang-eww-import-bookmarks-from-chrome-1
       (json-read-file bookmark-file)))
    (eww-write-bookmarks))

  ;; http://acidwords.com/posts/2017-12-01-distraction-free-eww-surfing.html
  (defun sanel-eww-more-readable ()
    "Makes eww more pleasant to use. Run it after eww buffer is loaded."
    (interactive)
    (setq eww-header-line-format nil)              ; removes page title
    (setq mode-line-format nil)                    ; removes mode-line
    (set-window-margins (get-buffer-window) 20 20) ; increases size of margins
    (redraw-display)                               ; apply mode-line changes
    (eww-reload))                                  ; apply eww-header changes
  :config
  (bind-key "M" #'chunyang-eww-toggle-image eww-mode-map)
  ;; Sync bookmarks every time from Chrome
  (chunyang-eww-import-bookmarks-from-chrome)
  ;; XXX Both Google & DuckDuckGo are currently bocked in China
  (setq eww-search-prefix "https://duckduckgo.com/html/?q=")
  ;; Eww doesn't support Javascript, but HTTPS version of Google requires it (?)
  ;; (setq eww-search-prefix "https://www.google.com.hk/search?q=")
  (setq eww-search-prefix "http://www.google.com.hk/search?q=")
  (setq eww-search-prefix "https://www.bing.com/search?q="))

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))

  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

(use-package shr
  :defer t
  :config
  (setq shr-use-fonts nil)
  (define-advice shr-tag-blockquote (:around (old-fun &rest args) bug-32277)
    "Work-around for bug#32277."
    (cl-letf (((symbol-function 'shr-mark-fill) #'ignore))
      (apply old-fun args))))

(use-package google-this
  :disabled t
  :ensure t
  :diminish google-this-mode
  :preface (defvar google-this-keybind (kbd "C-c G"))
  :init (google-this-mode))

(use-package devdocs
  :ensure t
  :commands devdocs-search)

(use-package web-search
  :ensure t
  :load-path "~/src/web-search.el"
  :bind ("M-s M-s" . web-search))

(use-package helm-chrome
  :ensure t
  :defer t
  :config
  (setq helm-chrome-file
        "~/Library/Application Support/Google/Chrome/Profile 1/Bookmarks"))

(use-package helm-chrome-history
  :load-path "~/src/helm-chrome-history"
  :about Search Chrome history with helm
  :commands helm-chrome-history
  :config
  (setq helm-chrome-history-file
        "~/Library/Application Support/Google/Chrome/Default/History"))

(use-package org-web-tools
  :ensure t
  :about View, capture, and archive Web pages in Org-mode
  :homepage https://github.com/alphapapa/org-web-tools
  :defer t)


;;; Music

(use-package vlc
  :about VLC control
  :load-path "~/src/vlc.el"
  :commands vlc-play)

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
  :defer t
  :config
  (setq bongo-default-directory "~/Music/网易云音乐/")
  (defun chunyang-bongo-library-setup ()
    (run-at-time
     0.1
     nil
     (lambda ()
       ;; Insert files into library
       (let ((bongo-insert-whole-directory-trees t))
         (bongo-insert-file bongo-default-directory))
       ;; Switch to playlist
       (with-current-buffer (bongo-playlist)
         ;; Random mode
         (bongo-sprinkle-mode)))))
  (add-hook 'bongo-library-mode-hook #'chunyang-bongo-library-setup)
  ;; Hide logo
  (setq bongo-logo nil)
  ;; Disable mode line
  (setq bongo-mode-line-indicator-mode nil)
  ;; Disable header line due to https://github.com/dbrock/bongo/issues/49
  (setq bongo-header-line-mode nil)
  ;; Hide these buffers, type M-x `bongo'
  (setq bongo-default-library-buffer-name " *Bongo Library*"
        bongo-default-playlist-buffer-name " *Bongo Playlist*")
  (defun bongo-kill-buffers ()
    (interactive)
    (mapc #'kill-buffer (bongo-buffers))))

(use-package volume
  :ensure t
  :defer t)

(defun helm-music-player ()
  "My little Music Player using helm."
  (interactive)
  (require 'helm)
  (helm :sources
        (helm-build-in-buffer-source "Music"
          :data
          (mapcar #'abbreviate-file-name
                  (split-string
                   (shell-command-to-string "find ~/Music -name '*.mp3'")
                   "\n"))
          :action
          (lambda (mp3)
            (shell-command (format "mpg123 '%s' &" (expand-file-name mp3)))))
        :buffer "*helm music*"))

(defun chunyang-download-lyric (song-name)
  (interactive "sSong name: ")
  (shell-command
   (format
    "curl -s 'http://gecimi.com/api/lyric/%s' | jq .result[0].lrc | xargs curl -s"
    song-name)))


;;; Dictionary

(use-package ldoce5
  :about Longman Dictionary of Contemporary English 5
  :load-path "~/src/ldoce5.el"
  :commands (ldoce5-lookup ldoce5-helm))

(use-package youdao-dictionary
  :load-path "~/src/youdao-dictionary.el"
  :ensure t
  :bind (("C-c y" . youdao-dictionary-search)
         ("C-c Y" . youdao-dictionary-search-at-point+)))

(use-package osx-dictionary
  :if *is-mac*
  :ensure t
  :bind ("C-c d" . osx-dictionary-search-pointer))

(use-package bing-dict
  :ensure t
  :defer t
  :preface
  (defun bing-dict-eldoc-documentation-function ()
    (let ((word (word-at-point)))
      ;; 太短的单词不查
      (when (and word (> (length word) 4))
        (bing-dict-brief word))
      nil))

  ;; 注意一次只有一个 eldoc mode backend 生效
  (define-minor-mode bing-dict-eldoc-mode
    "Use bing-dict as backend of eldoc."
    :lighter " Bing Dict"
    (if bing-dict-eldoc-mode
        (progn (setq-local eldoc-documentation-function
                           #'bing-dict-eldoc-documentation-function)
               (eldoc-mode +1))
      (setq-local eldoc-documentation-function #'ignore)
      (eldoc-mode -1))))

(use-package google-translate
  :ensure t
  :defer t
  :defines google-translate-translation-directions-alist
  :preface
  (defun chunyang-google-translate-web (query)
    "Launch Google Translate with Web Browser."
    (interactive
     (let* ((default (or (and (use-region-p)
                              (buffer-substring
                               (region-beginning) (region-end)))
                         (current-word)))
            (prompt (if default
                        (format "Google Translate (default %s): " default)
                      "Google Translate: ")))
       (list (read-string prompt nil nil default))))
    (browse-url (format "https://www.google.com/translate_t?text=%s"
                        (url-hexify-string query))))
  :init
  (eval-after-load 'google-translate-core
    '(setq google-translate-base-url "http://translate.google.cn/translate_a/single"
           google-translate-listen-url "http://translate.google.cn/translate_tts"))
  (eval-after-load 'google-translate-tk
    '(setq google-translate--tkk-url "http://translate.google.cn/"))
  (eval-after-load 'google-translate-smooth-ui
    ;; For M-x `google-translate-smooth-translate'
    '(setq google-translate-translation-directions-alist
           '(("en" . "zh-CN") ("zh-CN" . "en")))))

(use-package echo
  :commands echo-mode)

(use-package xinhua
  :about 《新华字典》
  :commands xinhua)

;; [[https://www.moedict.tw/about.html][萌典]] - 繁体 - 台湾
;; See also [[https://github.com/kuanyui/moedict.el][kuanyui/moedict.el: Moe Dictionary client for Emacs. 萌典 Emacs 版客戶端]]
(defun chunyang-moedict (query)
  (interactive
   (let ((char (char-after)))
     (list (read-string "萌典: "
                        (and char
                             (> (string-bytes (string char)) 1)
                             (string char))))))
  ;; TODO: Render the reuslt
  (shell-command (format "curl 'https://www.moedict.tw/uni/%s'" query)))

(use-package foldoc
  :about Emacs client for http://foldoc.org/
  :homepage https://github.com/xuchunyang/foldoc.el
  :load-path "~/src/foldoc.el"
  :config (setq foldoc-file (locate-user-emacs-file "var/folddoc/Dictionary"))
  :commands foldoc)


;;; English

;; https://github.com/manateelazycat/company-english-helper
;; company-ispell.el

(use-package longman-3000
  :commands
  longman-3000-insert
  longman-3000-helm
  longman-3000-company)

(use-package helm-english-helper
  :commands helm-english-helper)

(use-package helm-baidu-dict
  :homepage https://dict.baidu.com
  :about 百度汉语 - 查询成语和诗词
  :commands helm-baidu-dict-suggest)

(use-package helm-baidu-fanyi
  :homepage https://fanyi.baidu.com
  :about 百度翻译
  :commands helm-baidu-fanyi-suggest)

(use-package helm-suggest
  :load-path "~/src/helm-suggest"
  :homepage https://github.com/xuchunyang/helm-suggest
  :commands (helm-suggest-baidu-dict
             helm-suggest-baidu-fanyi
             helm-suggest-ldoceonline
             helm-suggest-douban-book
             helm-suggest-commandlinefu))

(use-package wordnut
  :about Interface to WordNet https://wordnet.princeton.edu/
  :notes brew install wordnet
  :commands wordnut-search)


;;; AppleScript

(use-package applescript-mode
  :ensure t
  :defer t
  :preface
  (defun chunyang-applescript-mode-setup ()
    (setq tab-width 4))
  :config
  (add-hook 'applescript-mode-hook #'chunyang-applescript-mode-setup))

(use-package ob-applescript
  :homepage https://github.com/stig/ob-applescript.el
  :ensure t
  :after org)


;;; Shell (including shell-command, shell, term and Eshell)

(use-package flycheck-checkbashisms    ; Don't use Bash-only features in /bin/sh
  :disabled Use ShellCheck instead
  :homepage https://github.com/Gnouc/flycheck-checkbashisms
  :notes
  - (executable-find "checkbashisms")
  - To install checkbashisms, see
  "https://github.com/Gnouc/flycheck-checkbashisms#install-checkbashisms"
  :ensure t
  :after flycheck
  :config (flycheck-checkbashisms-setup))

(use-package bats-mode
  :ensure t
  :defer t
  :about Bash Automated Testing System
  :homepage https://github.com/sstephenson/bats
  :notes
  - (man "1 bats")
  - (man "7 bats"))

(use-package term
  :commands term
  :config
  ;; Allow more lines before truncating
  (setq term-buffer-maximum-size 10240)

  ;; Respect my own M-x
  (bind-key "M-x" (lookup-key global-map [?\M-x]) term-raw-escape-map)

  ;; C-c C-j   term-line-mode
  ;; C-c C-k   term-char-mode
  ;;
  ;; However, It's hard to remember and distinguish them, so just use C-c C-j to
  ;; toggle these two modes.
  (bind-key "C-c C-j" #'term-char-mode term-mode-map))

(use-package vterm
  :if module-file-suffix
  :homepage https://github.com/akermu/emacs-libvterm
  :ensure t
  :defer t
  :notes
  To build vterm-module.so
  - mkdir -p build
  - cd build
  - cmake ..
  - make)

(use-package shell-pop
  :ensure t
  :commands shell-pop)

(use-package pcmpl-git
  :disabled t
  :after pcomplete
  :load-path "~/src/pcmpl-git-el")

(use-package eshell-z
  :ensure t
  :defer t)

(use-package eshell
  :defer t
  :functions eshell-get-history
  :preface
  ;; FIXME: Rework this, it has bugs, e.g., $ ls ~/Library/Application\ Scripts
  (defun chunyang-eshell-insert-last-arg ()
    "Insert the (rough) last arg of the last command, like ESC-. in shell."
    (interactive)
    (with-current-buffer eshell-buffer-name
      (let ((last-arg
             (car (last
                   (split-string
                    (substring-no-properties (eshell-get-history 0)))))))
        (and last-arg (insert last-arg)))))
  :bind ("C-x M" . eshell)              ; 'C-x m' runs `compose-mail' by default
  :init
  (setq eshell-banner-message
        '(concat (mapconcat #'identity (mingju) " -- ")
                 "\n\n"))
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
              ;; watch - execute a program periodically, showing output fullscreen
              (add-to-list 'eshell-visual-commands "watch")
              ;; Setup Plan9 smart shell
              ;; (require 'em-smart)
              ;; (eshell-smart-initialize)
              (bind-keys :map eshell-mode-map
                         ("C-c C-q" . eshell-kill-process)
                         ("M-."     . chunyang-eshell-insert-last-arg))
              (eshell/export "EDITOR=emacsclient -n")
              (eshell/export "VISUAL=emacsclient -n")
              ;; Disable scroll, see
              ;; https://emacs.stackexchange.com/questions/28819/eshell-goes-to-the-bottom-of-the-page-after-executing-a-command
              (remove-hook 'eshell-output-filter-functions
                           'eshell-postoutput-scroll-to-bottom)

              (require 'eshell-z)))


  (use-package eshell-git-prompt
    :ensure t
    :defer t
    ;; :init
    ;; Needed at least for `eshell-git-prompt'?
    ;; (setq eshell-highlight-prompt nil)
    ;; :config (eshell-git-prompt-use-theme 'powerline)
    )

  (use-package eshell-prompt-extras
    :disabled t
    :ensure t
    :config
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda))

  (use-package chunyang-eshell-ext))

(use-package eshell-did-you-mean
  :disabled t
  :ensure t
  :defer t
  :init
  (autoload 'eshell-did-you-mean-setup "eshell-did-you-mean")
  (with-eval-after-load 'eshell
    (eshell-did-you-mean-setup)))


;;; Emacs + Shell

(use-package $
  :about https://emacs-china.org/t/xiki-shell/9448
  :commands $
  ;; :bind ("M-RET" . $)
  )

(define-advice shell-command (:after (&rest _) exchange-point-and-mark)
  "Move point to the end of the inserted output.
Because I usualy want to delete the final trailing newline."
  (when (and (eq this-command 'shell-command)
             current-prefix-arg)
    (exchange-point-and-mark t)))

;; 1) Emacs should load `chunyang-shell.el'
;; 2) Shell in external Terminal should load misc/linux.sh or misc/mac.sh
(use-package chunyang-shell
  :commands (shell/info                 ; For shell in Terminal (outside Emacs)
             helm-bash-history
             chunyang-concat-shell-command))

;; For "emacsclient --eval EXPR". In EXPR, one has to use `setq' to modify this
;; variable temporarily, for example,
;;
;; emacsclient --eval "(progn (setq server-eval-and-how-to-print 'buffer) (org-agenda-list))"
;;
;; `let' will not work.
;; TODO: Silent if success, otherwise report problem? `with-demoted-errors'
(defcustom server-eval-and-how-to-print nil
  "Custom how to print in `server-eval-and-print'."
  :type '(choice
          (const :tag "Default (that is, print result of evaluation)" nil)
          (const :tag "Supress everything" 'supress)
          (const :tag "Print the current buffer" 'buffer))
  :group 'server)

(define-advice server-eval-and-print (:around (&rest r) how-to-print)
  "Decide how to print in `server-eval-and-print', according to `server-eval-and-how-to-print'."
  (let ((server-eval-and-how-to-print
         server-eval-and-how-to-print))
    (seq-let (_orig-fun expr proc) r
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

(use-package fish-mode
  :about Major mode for fish shell scripts
  :ensure t
  :defer t)


;;; Calendar

(use-package calendar
  :init
  ;; 每周从周一开始（需要 load 之前设置）
  (setq calendar-week-start-day 1)
  :defer t)

(use-package calfw
  :about "Alternative Calendar Interface"
  :homepage https://github.com/kiwanami/emacs-calfw
  :ensure t
  :commands cfw:open-calendar-buffer)

(use-package calfw-org
  :about "calfw interface for org agenda"
  :ensure t
  :commands cfw:open-org-calendar)


;;; Org mode

(use-package org
  :preface
  (defun chunyang-org-eval-defun ()
    "`eval-defun' wrapper for org-mode."
    (interactive)
    (let ((beginning-of-defun-function nil)
          (end-of-defun-function (lambda () (forward-sexp 1))))
      (call-interactively #'eval-defun)))

  (defun chunyang-org-info-lookup-symbol ()
    "Call `info-lookup-symbol' within a source edit buffer if needed."
    (interactive)
    (if (not (org-in-src-block-p))
        (call-interactively 'info-lookup-symbol)
      (org-babel-do-in-edit-buffer
       (save-excursion
         (call-interactively 'info-lookup-symbol)))
      (switch-to-buffer-other-window "*info*")))

  (defun chunyang-org-babel-open-tangle-file ()
    (interactive)
    (unless (org-in-src-block-p)
      (user-error "Point is not in a source code block"))
    (let* ((spec (car (cdr (car (org-babel-tangle-single-block 1 t)))))
           (get-spec (lambda (name) (cdr (assoc name (nth 4 spec)))))
           (tangle (funcall get-spec :tangle)))
      (when tangle
        (find-file tangle))))

  (defun chunyang-org-babel-highlight-result ()
    "Highlight the result of the current source block.
Adapt from `org-babel-remove-result'."
    (interactive)
    (let ((location (org-babel-where-is-src-block-result nil nil)))
      (when location
        (save-excursion
          (goto-char location)
          (when (looking-at (concat org-babel-result-regexp ".*$"))
            (pulse-momentary-highlight-region
             (1+ (match-end 0))
             (progn (forward-line 1) (org-babel-result-end))))))))

  (defun chunyang-org-babel-copy-previous-src-block ()
    ;; TODO 包括内容
    "复制上一个代码块（不包括内容）."
    (interactive)
    (let (result)
      (save-excursion
        (org-babel-previous-src-block)
        (let ((element (org-element-at-point)))
          (when (eq (car element) 'src-block)
            (let* ((pl (cadr element))
                   (lang (plist-get pl :language))
                   (switches (plist-get pl :switches))
                   (parms (plist-get pl :parameters)))
              (setq result
                    (format
                     (concat "#+begin_src %s\n"
                             "\n"
                             "#+end_src\n")
                     (mapconcat #'identity
                                (delq nil (list lang switches parms))
                                " ")))))))
      (and result (insert result))
      (previous-line 2)))

  (defun chunyang-org-babel-execute-python-in-iTerm ()
    (interactive)
    (seq-let (type plist) (org-element-at-point)
      (cond ((not (eq 'src-block type))
             (user-error "Not a src block"))
            ((not (string= "python" (plist-get plist :language)))
             (user-error "Not a Python src block"))
            (t
             (let ((filename (make-temp-file "" nil ".py")))
               (org-babel-tangle '(4) filename)
               (chunyang-mac-iTerm-send-string (concat "python " filename)))))))

  (defun chunyang-org-babel-execute-js-in-iTerm ()
    (interactive)
    (seq-let (type plist) (org-element-at-point)
      (cond ((not (eq 'src-block type))
             (user-error "Not a src block"))
            ((not (string= "js" (plist-get plist :language)))
             (user-error "Not a JS src block"))
            (t
             (let ((filename (make-temp-file "" nil ".js")))
               (org-babel-tangle '(4) filename)
               (chunyang-mac-iTerm-send-string (concat "node " filename)))))))
  :defer t
  :bind (("C-c c"      . org-capture)
         ("C-c a"      . org-agenda)
         ("C-c l"      . org-store-link)
         ;; Don't forget to use `transpose-lines' and `transpose-sexps'
         ("<M-S-down>" . org-drag-line-forward)
         ("<M-S-up>"   . org-drag-line-backward))
  :config
  (bind-key "C-M-x" #'chunyang-org-eval-defun org-mode-map)
  ;; Don't indent text under headings
  (setq org-adapt-indentation nil)
  ;; Keep indentation in src block on export
  (setq org-src-preserve-indentation t)
  ;; Fix TAB when point is on src block
  (setq org-src-tab-acts-natively t)

  (setq org-log-done 'time)

  (setq org-directory "~/Notes"
        org-agenda-files '("~/Notes"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "todo.org")
           "* TODO %i%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
           :empty-lines 1)
          ;; ("n" "Note" entry (file "notes.org")
          ;;  "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
          ;;  :empty-lines 1)
          ;; ("j" "Journal" plain (file+datetree "journal.org")
          ;;  "%?"
          ;;  :time-prompt t
          ;;  :empty-lines 1)
          ;; ("b" "Bookmark" entry (file "bookmarks.org")
          ;;  "* %?%(grab-mac-link 'chrome 'org)\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
          ;;  :empty-lines 1
          ;;  :immediate-finish t)
          ))

  (setq org-agenda-restore-windows-after-quit t)

  (defun chunyang-org-capture ()
    (interactive)
    (let* ((input
            (completing-read
             "org capture template:"
             (mapcar
              (lambda (template)
                (format "%s %s" (car template) (cadr template)))
              org-capture-templates)))
           (keys (car (split-string input))))
      (org-capture nil keys)))

  (add-hook 'org-agenda-mode-hook #'hl-line-mode)

  ;; Support link to Manpage, EWW and Notmuch
  (require 'org-man nil 'noerror)
  (require 'org-eww)
  (require 'org-notmuch nil 'noerror)

  (use-package ob-lisp                  ; Common Lisp
    :defer t
    :config
    ;; Requires SLY or SLIME, and the latter is used by default
    (setq org-babel-lisp-eval-fn 'sly-eval))

  (use-package ob-ipython
    ;; XXX org-capture: Capture abort: (json-readtable-error 47)
    ;; 作者假设 jupyter 正常运行，不好
    :disabled
    :homepage https://github.com/gregsexton/ob-ipython
    :ensure t
    ;; :config
    ;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
    :defer t)

  (use-package ob-racket
    :homepage https://github.com/DEADB17/ob-racket
    :load-path "~/src/ob-racket"
    :config
    (add-to-list 'org-src-lang-modes (cons "racket" 'scheme)))

  (add-to-list 'org-src-lang-modes '("js" . js2))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk        . t)
     (clojure    . t)
     (C          . t)
     (ditaa      . t)
     (emacs-lisp . t)
     (eshell     . t)
     (latex      . t)
     (lisp       . t)
     (lua        . t)
     (org        . t)
     (perl       . t)
     (python     . t)
     (R          . t)
     (racket     . t)
     (ruby       . t)
     (scheme     . t)
     (shell      . t)
     (sql        . t)
     (sqlite     . t)))

  ;; Work-around for
  ;; http://lists.gnu.org/archive/html/emacs-orgmode/2018-03/msg00013.html
  (define-advice org-babel-expand-body:clojure (:filter-return (body) do)
    (format "(do %s)" body))

  ;; This is not safe
  (setq org-confirm-babel-evaluate nil)

  ;; Upcase #+begin_example...#+end_example in the results
  (setq org-babel-uppercase-example-markers t)

  ;; Highlight the result of source block
  (add-hook 'org-babel-after-execute-hook
            (defun chunyang-org-babel-highlight-result-maybe ()
              (when (eq this-command 'org-ctrl-c-ctrl-c)
                (chunyang-org-babel-highlight-result))))

  (define-advice org-babel-eval-wipe-error-buffer (:override () kill-buffer)
    "Just kill the buffer since the buffer/window is useless and annoying."
    (when (get-buffer org-babel-error-buffer-name)
      (kill-buffer org-babel-error-buffer-name)))

  ;; Or use C-c C-v C-x (`org-babel-do-key-sequence-in-edit-buffer') instead
  (bind-key "C-h S" #'chunyang-org-info-lookup-symbol org-mode-map)

  ;; [[https://emacs-china.org/t/topic/4540][rg搜索文字并跳转后如何自动展开上下文？ - Spacemacs - Emacs China]]
  (add-hook 'next-error-hook
            (defun chunyang-org-reveal-after-next-error ()
              (and (derived-mode-p 'org-mode) (org-reveal))))

  ;; https://emacs-china.org/t/topic/5494
  (setq org-protocol-protocol-alist
        '(("bookmark"
           :protocol "bookmark"
           :function chunyang-org-protocol-capture-bookmark)))

  (defun chunyang-org-protocol-capture-bookmark (_info)
    (org-capture nil "b")
    ;; Only needed for Mac Port's builtin protocol support
    (when *is-mac-port*
      (run-at-time 0 nil #'chunyang-mac-switch-back-to-previous-application))
    nil)

  (defun chunyang-mac-switch-back-to-previous-application ()
    (interactive)
    ;; http://blog.viktorkelemen.com/2011/07/switching-back-to-previous-application.html
    (do-applescript
     (mapconcat
      #'identity
      '("tell application \"System Events\""
        "  tell process \"Finder\""
        "    activate"
        "    keystroke tab using {command down}"
        "  end tell"
        "end tell")
      "\n")))

  ;; $ emacsclient 'org-protocol://bookmark?'
  (require 'org-protocol)
  (require 'org-habit))

(use-package ox-html
  :defer t)

(use-package chunyang-org
  :commands (chunyang-org-agenda-csv
             helm-org-easy-templates
             chunyang-org-format-region-as-code-block
             chunyang-org-preview-via-pandoc
             chunyang-org-babel-tangle))

(use-package toc-org
  :homepage https://github.com/snosov1/toc-org
  :notes "Add TOC tag to a heading then 'M-x toc-org-insert-toc'"
  :ensure t
  :defer t)

(use-package grab-mac-link
  :if *is-mac*
  :load-path "~/src/grab-mac-link"
  :ensure t
  :commands (grab-mac-link grab-mac-link-dwim)
  :config (setq grab-mac-link-dwim-favourite-app 'chrome))

(use-package htmlize                    ; Enable src block syntax
                                        ; highlightting during
                                        ; exporting from org to html
  :ensure t
  :defer t)

(use-package orglink
  :disabled t
  :ensure t
  :preface
  ;; XXX To fix "funcall-interactively: Text is read-only" error, when
  ;; entering pattern after M-x el-search-pattern
  (defun chunyang-orglink-turn-on-maybe ()
    (unless (minibufferp)
      (orglink-mode)))
  :defer t
  :init (add-hook 'prog-mode-hook #'chunyang-orglink-turn-on-maybe)
  :config (setq orglink-mode-lighter nil))

;; TODO: Learn more about this package
(use-package org-board                  ; Bookmark with Org
  :ensure t
  :defer t)

(use-package habitica
  :disabled t
  :ensure t
  :defer t)

(use-package org-noter
  :about Emacs document annotator, using Org-mode
  :homepage https://github.com/weirdNox/org-noter
  :ensure t
  :defer t
  :config (setq org-noter-always-create-frame nil))


;;; C

;; C Programming Tools:
;; - GDB (info "(gdb) Top")
;; - GCC (info "(gcc) Top")
;; - CC Mode (info "(ccmode) Top")
;;
;; Note that it's not easy to use GDB on macOS (blame Apple).

(use-package cc-mode
  ;; Tips:
  ;; - C-M-a/e and M-a/e understands functions and statements, it's cool
  :defer t
  :init
  ;; Turn on Auto-newline and hungry-delete-key, they are adviced by cc-mode
  ;; manual, let me try them for a while. BTW, 'a' and 'h' will be indicated in
  ;; the mode-line, such as C/lah.  BTW, 'l' stands for electric keys, use C-c
  ;; C-l to toggle it.
  ;; (add-hook 'c-mode-common-hook #'c-toggle-auto-hungry-state)

  (defun chunyang-c-mode-common-setup ()
    (abbrev-mode -1))

  (add-hook 'c-mode-common-hook #'chunyang-c-mode-common-setup)

  (defun chunyang-cpp-lookup ()
    "Lookup C function/macro/etc prototype via Preprocessing."
    (interactive)
    (let ((symbol (current-word))
          (buffer "*clang-cpp-output*")
          (file buffer-file-name))
      (with-current-buffer (get-buffer-create buffer)
        (erase-buffer)
        (call-process "cc" nil t nil "-E" file)
        (goto-char (point-min))
        (re-search-forward symbol nil t)
        (display-buffer (current-buffer)))))
  :config
  ;; Really need a key binding for reporting bug? M-x `c-submit-bug-report'
  (unbind-key "C-c C-b" c-mode-map)
  ;; C/*lah => C
  (advice-add 'c-update-modeline :around #'ignore)

  ;; Add imenu support for section comment like this
  ;; /*** includes ***/
  (add-to-list
   'cc-imenu-c-generic-expression
   (list "Section"
         (rx line-start "/***" (group (1+ not-newline)) "***/" line-end)
         1)))

(use-package irony
  :disabled t
  :ensure t
  :defer t
  :init
  (add-hook 'c-mode-hook #'irony-mode)

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (bind-key
     [remap completion-at-point]
     #'irony-completion-at-point-async
     irony-mode-map)
    (bind-key
     [remap complete-symbol]
     #'irony-completion-at-point-async
     irony-mode-map))
  (add-hook 'irony-mode-hook #'my-irony-mode-hook)
  (add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

  :config
  (use-package company-irony
    :ensure t
    :defer t
    :init (with-eval-after-load 'company
            (add-to-list 'company-backends 'company-irony))
    :config
    ;; Prefer "fun ()" over "fun()"
    (define-advice company-irony--post-completion
        (:override (candidate) put-space-open-parentheses-maybe)
      "Add one space before open-parenthesis if using GNU style."
      (when (and (equal c-indentation-style "gnu")
                 candidate)
        (let ((point-before-post-complete (point)))
          (if (irony-snippet-available-p)
              (irony-completion-post-complete candidate)
            (let ((str (irony-completion-post-comp-str candidate)))
              ;; Prefer GNU C style by adding one space after function
              ;; name (2016-10-24 by xcy)
              (unless (string-empty-p str)
                (insert " "))
              (insert str)
              (company-template-c-like-templatify str)))
          (unless (eq (point) point-before-post-complete)
            (setq this-command 'self-insert-command))))))

  (use-package irony-eldoc        ; Note: this does not work very well
    :ensure t
    :defer t
    :init (add-hook 'irony-mode-hook #'irony-eldoc)))

(use-package flycheck-irony
  :ensure t
  :after irony
  :config
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


;;; Rust

(use-package rust-mode
  :ensure t
  :defer t)

(use-package racer
  :ensure t
  :homepage https://github.com/racer-rust/emacs-racer
  :notes
  - $ cargo install racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust     ; The built-in check of Flycheck not working well
  :ensure t
  :defer t
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package ob-rust
  :homepage https://travis-ci.org/micanzhang/ob-rust
  :ensure t
  :after org)

(use-package toml-mode
  :ensure t
  :defer t)


;;; Common Lisp

(use-package slime
  :disabled t
  :ensure t
  :defer t
  :init (setq inferior-lisp-program "sbcl"))

;; TODO eldoc support (`sly-autodoc-mode') is not working
;; TODO Is completion working? (best with Company support, usually it should work out of box)
(use-package sly
  :disabled t   ; Since I don't write Common Lisp and this package updates a lot
  :ensure t
  :defer t
  :preface
  (defun chunyang-sly-eval-print-last-sexp ()
    "Like `chunyang-eval-print-last-sexp' in Emacs Lisp.
Note that `sly-eval-last-expression' with prefix argument
provides similiar function."
    (interactive)
    (let ((string (sly-last-expression)))
      (sly-eval-async `(slynk:eval-and-grab-output ,string)
        (lambda (result)
          (cl-destructuring-bind (_output value) result
            (unless (chunyang-current-line-empty-p) (insert ?\n))
            ;; (insert "     ⇒ " value)
            (insert "     => " value)
            (unless (chunyang-current-line-empty-p) (insert ?\n)))))))

  (defun chunyang-sly-eval-print-last-sexp-in-comment ()
    (interactive)
    (let ((string (sly-last-expression)))
      (sly-eval-async `(slynk:eval-and-grab-output ,string)
        (lambda (result)
          (cl-destructuring-bind (_output value) result
            (comment-dwim nil)
            (insert (format " => %s" value)))))))
  :config
  (setq inferior-lisp-program "sbcl")
  (bind-key "C-j" #'chunyang-sly-eval-print-last-sexp sly-mode-map)
  (bind-key "C-j" #'chunyang-sly-eval-print-last-sexp-in-comment sly-mode-map))

(use-package sly-mrepl
  :no-require t                      ; Silence byte-compiling warnning
  :defer t
  :config
  ;; Enable Paredit in REPL too
  (add-hook 'sly-mrepl-mode-hook #'paredit-mode))


;;; newLISP <http://www.newlisp.org/>

(use-package newlisp-mode
  :disabled Breaks indentation, see https://emacs-china.org/t/emacs-lisp-if/9394
  :ensure t
  :homepage https://github.com/kosh04/newlisp-mode
  :defer t)


;;; Ruby

(use-package ruby-mode
  :defer t
  :init
  (add-hook 'ruby-mode-hook #'superword-mode))

(use-package inf-ruby
  :ensure t
  ;; `package.el' does the setup via autoload
  :defer t)

(use-package robe
  ;; NOTE: Some gems have to be installed before using, see
  ;;       https://github.com/dgutov/robe
  :ensure t
  :after ruby-mode
  :config
  (add-hook 'ruby-mode-hook #'robe-mode))

(use-package ruby-tools
  :disabled t
  :ensure t
  :defer t
  :init (add-hook 'ruby-mode-hook #'ruby-tools-mode))


;;; Standard ML

(use-package sml-mode
  :ensure t
  :defer t)

(use-package sml-eldoc
  :disabled t                           ; Not working
  :commands sml-eldoc-turn-on
  :init (add-hook 'sml-mode-hook #'sml-eldoc-turn-on))

(use-package ob-sml
  :ensure t
  :after org)


;;; Scheme

(use-package geiser                     ; For Scheme
  :ensure t
  :defer t
  :defines geiser-mode-map
  :preface
  (defun chunyang-geiser-eval-print-last-sexp ()
    (interactive)
    (let ((message-log-max nil))
      ;; FIXME: Handle error correctly like C-u M-x `geiser-eval-last-sexp'
      (let ((res (geiser-eval-last-sexp nil)))
        (unless (string= "=> " res)
          (unless (bolp) (insert "\n"))
          (insert
           ";; "
           ;; Handle Multiple Values
           (replace-regexp-in-string "\n=>" "\n;; =>" res)
           "\n")
          (message nil)))))

  (defun chunyang-geiser-expand-sexp-at-point (&optional all)
    (interactive "P")
    (geiser-expand-region (point)
                          (save-excursion (forward-sexp) (point))
                          all
                          t))

  ;; Important keys:
  ;; C-c C-z - Switch between source and REPL
  ;; C-z C-a - Switch to REPL with current module
  (defun chunyang-geiser-setup ()
    (bind-keys :map geiser-mode-map
               ("C-h ."   . geiser-doc-symbol-at-point)
               ("C-h C-." . geiser-doc-look-up-manual)
               ("C-j"     . chunyang-geiser-eval-print-last-sexp)
               ("C-c e"   . chunyang-geiser-expand-sexp-at-point)))
  :config
  ;; To learn how Geiser chooses Scheme implementation,
  ;; see (info "(geiser) The source and the REPL")
  ;; (setq geiser-active-implementations '(racket chicken))

  ;; XXX With scheme src block in Org, `scheme-mode' is called from time to
  ;; time, then `geiser-mode' is called, but it can't figure out the scheme
  ;; implementation.
  ;;
  ;; 1. In the Org mode, there is no need to enable `geiser-mode'.
  ;; 2. In C-c ' (`org-src-mode'), `geiser-mode' should be on and with correct
  ;;    scheme implementation.
  ;;
  ;; Ok, for now, just use the fallback.
  (setq geiser-default-implementation 'guile)

  (add-hook 'geiser-mode-hook #'chunyang-geiser-setup)

  ;; Yes, use ParEdit in the REPL too
  (add-hook 'geiser-repl-mode-hook #'paredit-mode)

  ;; not sure why it defaults to "scheme"
  (setq geiser-chez-binary "chez")

  ;; (info "(geiser) Seeing is believing")
  (and *is-mac* (setq geiser-image-viewer "open")))

(use-package ob-scheme
  :defer t
  :config
  (define-advice org-babel-scheme-get-repl (:around (old-fun &rest args) dont-switch-buffer)
    "Work-around for URL `https://github.com/jaor/geiser/issues/107'."
    (cl-letf (((symbol-function 'geiser-repl--switch-to-buffer) #'set-buffer))
      (apply old-fun args))))

(use-package scheme
  :defer t
  :config (add-hook 'scheme-mode-hook #'paredit-mode))

(use-package sicp
  :homepage https://www.neilvandyke.org/sicp-texi/
  :ensure t
  :defer t)

;; Dependency of `racket-mode'
(use-package faceup
  :ensure t
  :defer t)

(use-package racket-mode
  :homepage https://github.com/greghendershott/racket-mode
  :ensure t
  ;; :load-path "~/src/racket-mode"
  :defer t
  :mode "\\.rkt\\'"
  :init
  ;; Might not be a very good idea, because '#lang basic' etc
  (add-hook 'racket-mode-hook #'paredit-mode)
  (defun chunyang-racket-mode-setup ()
    ;; `racket-mode' enable this mode, not sure why
    (and (bound-and-true-p hs-minor-mode)
         (hs-minor-mode -1)))
  (add-hook 'racket-mode-hook #'chunyang-racket-mode-setup)
  (defun chunyang-racket-describe-mode-setup ()
    (run-at-time
     0
     nil
     (lambda ()
       "Remove the last line."
       (when-let ((buffer (get-buffer "*Racket Describe*")))
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-max))
             (goto-char (line-beginning-position))
             (when (looking-at-p "^Definition")
               (let ((inhibit-read-only t))
                 (delete-region (line-beginning-position)
                                (line-end-position))))))))))
  (add-hook 'racket-describe-mode-hook #'chunyang-racket-describe-mode-setup)
  :config
  (define-advice racket-describe (:around (old-fun &rest args) silence)
    "Silence `message', which is annoying."
    (let ((message-log-max nil))
      (apply old-fun args)
      (message nil)))
  ;; For `racket-shell-send-string-no-output'
  (require 'racket-ext)
  (defun chunyang-racket-eval-print-last-sexp ()
    (interactive)
    (let* ((end (point))
           (beg (save-excursion
                  (backward-sexp)
                  (if (save-match-data (looking-at "#;"))
                      (+ (point) 2)
                    (point))))
           (sexp (buffer-substring-no-properties beg end))
           (str (replace-regexp-in-string (rx (* "\n") eos) "\n" sexp))
           ;; XXX error?
           (res (racket-shell-send-string-no-output str)))
      (unless (bolp) (insert ?\n))
      (cond ((string= res "")
             ;; (message "No return value for this sexp")
             )
            ((string-match ".\n+." res) ; Multiline
             (insert res))
            (t
             (insert ";; => " res "\n")))))

  (bind-key "C-j" #'chunyang-racket-eval-print-last-sexp racket-mode-map)

  (defun chunyang-racket-macro-expand ()
    "Expand the macro form following point."
    (interactive)
    (save-excursion
      (forward-sexp)
      (racket-expand-last-sexp)))
  (bind-key "C-c e" #'chunyang-racket-macro-expand racket-mode-map)

  (bind-key "C-h ." #'racket-describe racket-mode-map))

(use-package scribble-mode
  :about https://docs.racket-lang.org/scribble/index.html
  :homepage https://github.com/emacs-pe/scribble-mode
  :ensure t
  :defer t)

;; NOTE: scribble.el and scribble-mode.el are two different package,
;; though they are supposed to provide the same function, they can't
;; be used at the same emacs session
(use-package scribble
  :disabled
  :homepage http://www.neilvandyke.org/scribble-emacs/
  :load-path "~/src/emacs-scribble"
  :mode ("\\.scrbl\\'" . scribble-mode))

;; https://janet-lang.org/
(use-package janet-mode
  :ensure t
  :homepage https://github.com/ALSchwalm/janet-mode
  :preface
  (define-derived-mode inferior-janet-mode comint-mode "Inferior Janet"
    "Major mode for Janet inferior process."
    ;; FIXME THIS IS INCORRECT AND NOT WORKING
    ;; janet:0:>
    ;; janet:10:(>
    ;; janet:23:((>
    ;; (setq comint-prompt-regexp (rx bol "janet:" (+ num) ":" (* "(") "> "))
    (setq-local comint-prompt-read-only t))

  (defun run-janet ()
    (interactive)
    (with-current-buffer (make-comint "Janet" "janet" nil "-s")
      (unless (eq major-mode 'inferior-janet-mode)
        (inferior-janet-mode))
      (display-buffer (current-buffer))))
  :defer t)


;;; Web

(use-package sgml-mode
  :preface
  (defun chunyang-html-mode-setup ()
    ;; Add HTML Empty Elements.  XHTML requires /> but HTML doesn't
    (add-to-list 'sgml-empty-tags "source"))
  :hook (html-mode . chunyang-html-mode-setup))

(use-package web-mode
  :disabled t
  :homepage http://web-mode.org
  :ensure t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset    2
        web-mode-code-indent-offset   2))

(use-package emmet-mode
  :disabled t
  :homepage https://github.com/smihica/emmet-mode
  :about Unfold CSS-selector-like expressions to markup
  :ensure t
  :defer t)

(use-package expand-emmet
  :load-path "~/.emacs.d/lisp/expand-emmet"
  :commands expand-emmet-line)

(use-package js
  :defer t
  :preface
  (defun chunyang-nodejs-find-module (module)
    (interactive
     (list (read-string "Module: "
                        ;; Use `syntax-ppss' or text props?
                        (thing-at-point 'symbol))))
    (let (filename)
      (with-temp-buffer
        (let ((exit (call-process "node" nil t nil "-pe" (format "require.resolve('%s')" module))))
          (when (eq (char-before) ?\n)
            (delete-char -1))
          (if (zerop exit)
              (setq filename (buffer-string))
            (user-error "Can't find location of %s" module))))
      (if (file-exists-p filename)
          (find-file filename)
        (user-error "Don't know how to open %s" filename))))
  :config
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (defun chunyang-js-mode-setup ()
    (setq electric-layout-rules
          (seq-remove (lambda (elt) (= (car elt) ?\;))
                      electric-layout-rules)))
  (add-hook 'js-mode-hook #'chunyang-js-mode-setup))

(use-package js2-mode
  :homepage https://github.com/mooz/js2-mode/
  :ensure t
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-mode-hide-warnings-and-errors))
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  :config (setq js2-skip-preprocessor-directives t))

(use-package nodejs-repl
  :ensure t
  :commands nodejs-repl-switch-to-repl)

(use-package tern
  :disabled
  :homepage http://ternjs.net/
  :ensure t
  :defer t
  :config
  ;; https://discuss.ternjs.net/t/emacs-get-the-full-function-docs-comments-with-c-c-c-d/50/3
  (defun tern-describe ()
    (interactive)
    (tern-run-query
     (lambda (data)
       ;; url, doc, type, origin
       (let-alist data
         (with-current-buffer (get-buffer-create "*Tern Describe*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (when .doc
               (insert .doc)
               (fill-region (point-min) (point-max)))
             (when .url
               (and .doc (insert "\n\n"))
               (insert .url))
             (goto-char (point-min))
             (display-buffer (current-buffer))))))
     '((type . "documentation") (docFormat . "full"))
     (point)))
  :hook (js2-mode . tern-mode))

(use-package company-tern
  :ensure t
  :after (company tern)
  :homepage https://github.com/proofit404/company-tern
  :init (add-to-list 'company-backends 'company-tern)
  :defer t)

(use-package js2-refactor
  :ensure t
  :defer t)

(use-package indium
  :disabled
  :ensure t
  :homepage https://github.com/NicolasPetton/indium
  :about JavaScript Awesome Development Environment
  :info (info "(Indium) Top")
  :hook (js2-mode . indium-interaction-mode))

(use-package tide
  :disabled t
  :homepage https://github.com/ananthakumaran/tide
  :hook (js2-mode . tide-setup))

(use-package skewer-mode
  :about live browser JavaScript, CSS, and HTML interaction
  :homepage https://github.com/skeeto/skewer-mode
  :disabled t
  :ensure t
  :defer t
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode)
  (add-hook 'html-mode-hook #'skewer-html-mode))

(use-package css-mode
  :defer t
  :config
  ;; 4 is already the default
  (setq css-indent-offset 4))

;; TODO Try this package (examples, documentation)
(use-package web-server
  :homepage https://github.com/eschulte/emacs-web-server
  :ensure t
  :preface
  (defun simple-http-server (root-dir)
    "简单的文件 HTTP 服务器，效果类似于 python -m http.server 8888."
    (interactive (list (expand-file-name default-directory)))
    (ws-start
     `(lambda (request)
        (with-slots ((proc process) headers) request
          (let* ((path (alist-get :GET headers))
                 (path (concat ,root-dir path)))
            (cond ((file-directory-p path)
                   (ws-response-header proc 200 (cons "Content-type" "text/html"))
                   (process-send-string
                    proc
                    (mapconcat
                     (lambda (f)
                       (when (file-directory-p f)
                         (setq f (concat f "/")))
                       (format "<li><a href=%s>%s</li>"
                               (url-encode-url f) (url-encode-url f)))
                     (directory-files path)
                     "\n")))
                  ((file-regular-p path)
                   (ws-send-file proc path))
                  (t
                   (ws-send-404 proc)))
            )))
     8888)
    (browse-url "http://localhost:8888/"))
  :defer t)

;; TODO: try this (by nullprogram's author)
(use-package simple-httpd
  :ensure t
  :defer t
  :homepage https://github.com/skeeto/emacs-http-server)

;; TODO: 自己实现一个 HTTP 服务器（先写个文件服务器）


;;; Elixir

(use-package elixir-mode
  :ensure t
  :homepage https://github.com/elixir-lang/emacs-elixir
  :defer t
  :config
  (require 'comint)
  (define-derived-mode inferior-elixir-mode comint-mode "Inferior Elixir"
    "Major mode for Elixir inferior process."
    (setq comint-prompt-regexp (rx bol (or "iex" "...") "(" (1+ num) ") ")))

  (defun run-elixir ()
    (interactive)
    (with-current-buffer (make-comint-in-buffer "Elixir" "*Elixir*" "iex" nil)
      (inferior-elixir-mode)
      (display-buffer (current-buffer)))))

(use-package alchemist
  :homepage https://github.com/tonini/alchemist.el
  :ensure t
  :defer t)


;;; Clojure

(use-package clojure-mode
  :ensure t
  :defer t
  :config (add-hook 'clojure-mode-hook #'paredit-mode))

(use-package cider
  :homepage
  https://github.com/clojure-emacs/cider
  https://cider.readthedocs.io/en/latest
  :ensure t
  :defer t
  :init
  ;; Defaults to `slime' if `cider' is not already in `load-path'
  (setq org-babel-clojure-backend 'cider)
  :config
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)

  (bind-key "C-h ." #'cider-doc cider-mode-map)
  (bind-key "C-h ." #'cider-doc cider-repl-mode-map)

  (setq cider-repl-display-help-banner nil)
  (setq cider-repl-display-in-current-window t)
  (setq cider-repl-scroll-on-output nil)

  (setq cider-prompt-for-symbol nil)

  (define-advice cider-eldoc-format-function (:around (old-fun thing pos eldoc-info) docstring)
    "Show docstring for function as well."
    (concat
     (funcall old-fun thing pos eldoc-info)
     (when-let* ((doc (lax-plist-get eldoc-info "docstring"))
                 (doc-one-line (substring doc 0 (string-match "\n" doc))))
       (concat "  |  " (propertize doc-one-line 'face 'italic))))))


;;; Go

(use-package chunyang-go
  :commands (chunyang-go-doc-browse-url
             chunyang-godoc-gogetdoc
             chunyang-helm-go-packages
             chunyang-go-toggle-if))

(use-package go-mode
  :ensure t
  :homepage https://github.com/dominikh/go-mode.el
  :defer t
  :preface
  (defun chunyang-go-setup ()
    (when buffer-file-name
      ;; It's handy for learning Go
      (setq-local compile-command
                  (if (string-suffix-p "_test.go" buffer-file-name)
                      "go test -v"
                    (format "go run %s"
                            (shell-quote-argument
                             (file-name-nondirectory buffer-file-name)))))
      ;; Defaults to 8 which takes too much visual space
      (setq tab-width 4)))
  :init
  ;; eglot with gopls complains GOPATH is not set
  (setenv "GOPATH" (expand-file-name "~/go"))
  (add-hook 'go-mode-hook #'chunyang-go-setup)
  ;; eglot needs yasnippet to complete function arguments
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'eglot-ensure)
  :config
  (define-advice imenu-add-to-menubar (:override (_name) ignore)
    "`go-mode' call `imenu-add-to-menubar' but eglot with gopls have trouble with imenu.
And by the way, the menu bar on macOS is buggy.")
  (define-advice eglot-imenu (:override () ignore)
    (imenu-default-create-index-function))
  ;; goimports fixes both import and format
  (setq gofmt-command "goimports")
  ;; gogetdoc > godef
  ;; (setq godoc-at-point-function #'godoc-gogetdoc)
  (setq godoc-at-point-function #'chunyang-godoc-gogetdoc)

  (bind-key "C-c C-a" #'chunyang-helm-go-packages go-mode-map)
  (bind-key "C-h C-." #'dash-alfred-helm          go-mode-map)
  (bind-key "C-h f"   #'gds                       go-mode-map)
  (bind-key "C-h ."   #'godoc-at-point            go-mode-map))

(use-package go-dlv
  :ensure t
  :about Debug Go with Delve and GUD
  :commands dlv)

(use-package gds
  :about Go doc search
  :load-path "~/go/src/github.com/xuchunyang/gds"
  :commands gds
  :config

  (setq gds-url "http://localhost:8000/pkg/%s/#%s")

  (define-advice gds-browse-url (:around (old-fun &rest r) ensure-godoc-is-running)
    "Ensure server at `gds-url' is running, if it's not, start it."
    (condition-case err
        (let ((url-request-method "HEAD"))
          (url-retrieve-synchronously (apply #'format gds-url r)))
      (file-error
       (message "No server is running at %s, will start  godoc via prodigy" gds-url)
       (require 'prodigy)
       (prodigy-start-service (prodigy-find-service "Godoc Server"))
       (sit-for .5)))
    (apply old-fun r)
    (advice-remove 'gds-browse-url #'gds-browse-url@ensure-godoc-is-running)))

(use-package go-rename
  :about use gorename to rename
  :ensure t
  :defer t)

(use-package go-guru
  :about use guru to find definition/callere
  :ensure t
  :defer t)

(use-package go-errcheck
  :about use errcheck to ensure errors are checked
  :ensure t
  :defer t)

(use-package go-eldoc
  :disabled                             ; replaced by gopls
  :ensure t
  :about use gocode for eldoc
  :homepage https://github.com/syohex/emacs-go-eldoc
  :defer t
  :init (add-hook 'go-mode-hook #'go-eldoc-setup))

(use-package go-impl
  :ensure t
  :about impl generates method stubs for implementing an interface
  :homepage
  - https://github.com/syohex/emacs-go-impl
  - https://github.com/josharian/impl
  :defer t)

(use-package go-keyify
  :about "T{1, 2, 3} => T{A: 1, B: 2, C: 3}"
  :homepage https://github.com/dominikh/go-tools/tree/master/cmd/keyify
  :load-path "~/src/go-tools/cmd/keyify/"
  :commands go-keyify)

(use-package go-sodoff
  :about generate a return statement
  :after go-mode
  :defer t                              ; not real go-sodoff.el
  :preface
  (defun go-sodoff ()
    (interactive)
    (when (zerop
           (call-process-region
            nil nil
            "gosodoff" nil t nil
            "-pos" (number-to-string (1- (position-bytes (point))))))
      ;; clean up
      (when (looking-at-p "^$")
        (delete-char -1))
      (call-interactively #'indent-for-tab-command))))

(use-package go-dlv
  :about Debug Go
  :homepage
  - https://github.com/go-delve/delve
  - https://github.com/benma/go-dlv.el
  ;; FIXME: I don't need to debug Go for now, so leave this for another day
  :disabled)

(use-package company-go
  :disabled                             ; replaced by gopls
  :after go-mode
  :ensure t
  :homepage https://github.com/mdempsky/gocode/tree/master/emacs-company
  :about use gocode for completion
  :config
  (add-to-list 'company-backends 'company-go))

(use-package ob-go
  :homepage https://github.com/pope/ob-go
  :ensure t
  :after org)


;;; Python

(use-package python
  :defer t
  :preface
  (defun chunyang-python-mode-setup ()
    (eldoc-mode -1)
    (kill-local-variable 'completion-at-point-functions))

  (defun chunyang-inferior-python-mode-setup ()
    (setq-local comint-process-echoes t)
    (kill-local-variable' completion-at-point-functions))
  :config
  (add-hook 'python-mode-hook #'chunyang-python-mode-setup)
  (add-hook 'inferior-python-mode-hook #'chunyang-inferior-python-mode-setup)

  (setq python-indent-guess-indent-offset nil)

  (setq python-shell-interpreter "python3"
        python-shell-completion-native-enable nil
        python-shell-font-lock-enable nil))

(use-package chunyang-python
  :commands (chunyang-jedi
             chunyang-python-comment-box))

(use-package elpy
  :disabled       ; too many dependencies (find-file-in-project ->
                  ; ivy), and I don't write python for now.
  :ensure t
  :defer t
  :init
  (defun chunyang-elpy-enable ()
    (elpy-enable)
    (elpy-mode)
    (remove-hook 'python-mode-hook #'chunyang-elpy-enable))
  (add-hook 'python-mode-hook #'chunyang-elpy-enable)
  :config
  (setq elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc))
  (bind-key "C-h ." #'elpy-doc elpy-mode-map)
  (setq elpy-shell-use-project-root nil))

(use-package pydoc
  :ensure t
  :commands pydoc
  :config (setq pydoc-command "python3 -m pydoc"))

(use-package helm-pydoc
  :ensure t
  :defer t)

(use-package pipenv
  :disabled
  :ensure t
  :defer t
  :hook (python-mode . pipenv-mode))

(use-package anaconda-mode
  :disabled
  :ensure t
  :defer t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

(use-package company-anaconda
  :disabled
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))


;;; Lua

(use-package lua-mode
  :ensure t
  :defer t)

(use-package company-lua
  :ensure t
  :after lua-mode
  :config
  (add-to-list 'company-backends 'company-lua))


;;; Misc

(use-package view
  :defer t
  :config (setq view-inhibit-help-message t))

(use-package image
  :defer t
  :config
  ;; + is hard to type
  (define-key image-map "=" 'image-increase-size))

(use-package ascii-art-to-unicode
  :ensure t
  :defer t
  :init
  ;; `aa2u' is hard to recall
  (defalias 'ascii-art-to-unicode 'aa2u))

(use-package restart-emacs :ensure t :defer t)

(use-package package-utils :ensure t :defer t)

(use-package e2ansi                     ; Provide Syntax Highlight for shell by
                                        ; Emacs.  This is very cool.
  :ensure t
  :load-path "~/src/e2ansi"
  :defer t)

(use-package adafruit-wisdom
  :about Get/display adafruit.com quotes (名人名言)
  :homepage https://github.com/gonewest818/adafruit-wisdom.el
  :ensure t
  :defer t)

(use-package graphviz-dot-mode
  :ensure t
  :defer t)


;;; IM

(use-package gitter
  :ensure t
  :defer t
  :config
  (setq gitter--debug t))


;;; News

(use-package hn
  :commands list-hacker-news)


;;; Programming Language

(use-package language-detection
  :ensure t
  :defer t
  :preface
  (defun chunyang-language-detection-region (b e)
    (interactive "r")
    (message "Language: %s"
             (language-detection-string (buffer-substring b e)))))


;;; Emacs

;; FIXME brew install emacs-mac
(unless (and source-directory
             (file-exists-p source-directory))
  (setq source-directory "~/src/emacs"))

(use-package elisp-bytecode
  :homepage "https://github.com/rocky/elisp-bytecode"
  :init (add-to-list 'Info-directory-list "~/src/elisp-bytecode")
  :defer t)


;;; Chinese | 中文

(use-package mingju
  :load-path "~/src/mingju"
  :commands mingju)

(use-package @300
  :load-path "~/src/300"
  :commands (@300 @300-random))

(use-package chunyang-chinese
  :commands (chunyang-chinese-insert-mark
             chinese-punctuation-mode
             chunyang-pinyin-occur))

;; macOS 下，使用官方 GUI Emacs 和系统自带的拼音输入法时，输入期间，在
;; Emacs buffer 已出现字母会随着输入的进行而发生“抖动”，相关讨论：
;; - https://emacs-china.org/t/mac-gui-emacs/186
;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=+23412

;; 临时解决方法，有效但不清楚有没有副作用
;; (setq redisplay-dont-pause nil)

(use-package opencc
  :ensure t
  :defer t)

(use-package scws
  :if module-file-suffix
  :about "SCWS 的 Emacs Module | 中文分词"
  :load-path "~/src/emacs-scws"
  :commands (scws scws-word-at-point))

(use-package pinyin
  :homepage https://github.com/xuchunyang/pinyin.el
  :load-path "~/src/pinyin.el"
  :commands pinyin)

(use-package moedict
  ;; Package-Requires: ((emacs "24.3") (helm "1.9.1") (esqlite "0.3.1"))
  :ensure esqlite
  :homepage https://github.com/kuanyui/moedict.el
  :load-path "~/src/moedict.el"
  :commands moedict)

;; https://cc-cedict.org/editor/editor.php?handler=QueryDictionary
(use-package cc-cedict
  :load-path "~/src/cc-cedict.el"
  :commands cc-cedict)


;;; Fun

(use-package sparkline
  :ensure t
  :about 生成走势图 | https://en.wikipedia.org/wiki/Sparkline
  :homepage https://github.com/woudshoo/sparkline
  :defer t)

(use-package spark
  :ensure t
  :about ▁▂▃▅▂█
  :homepage https://github.com/alvinfrancis/spark
  :defer t)

(use-package hr
  :about <hr> for Emacs
  :commands hr-insert)

(use-package github-emojis
  :commands (github-emojis-find-file github-emojis-helm))

(use-package lolcat
  :load-path "~/src/lolcat.el"
  :commands (eshell/lolcat
             lolcat-message
             lolcat-this-buffer
             lolcat-view-buffer
             lolcat-view-file))

(use-package svg-clock
  :disabled t                           ; Disabled because it will install the
                                        ; outdated svg-1.0.el from gnu elpa,
                                        ; rather then use the newer (not
                                        ; version) builtin one
  :ensure t
  :commands svg-clock)

(use-package zone-nyan
  :ensure t
  :about "Walk a cat (using svg)"
  :homepage https://github.com/wasamasa/zone-nyan
  :commands zone-mode)

;; My profile: https://codestats.net/users/xuchunyang
(use-package code-stats
  :disabled t
  :homepage https://codestats.net/
  :load-path "~/src/code-stats-emacs"
  :diminish code-stats-mode
  :config
  ;; (setq code-stats-url "https://beta.codestats.net")
  (add-hook 'prog-mode-hook #'code-stats-mode)
  (run-with-idle-timer 30 t #'code-stats-sync)
  (add-hook 'kill-emacs-hook (lambda () (code-stats-sync :wait))))

(use-package fortune
  :commands (fortune fortune-message)
  :config
  (cond (*is-mac*
         ;; On macOS, fortune is installed via Homebrew
         (setq fortune-dir  "/usr/local/share/games/fortunes/"
               fortune-file "/usr/local/share/games/fortunes/fortunes"))
        (*is-gnu-linux*
         (setq fortune-dir  "/usr/share/games/fortunes/"
               fortune-file "/usr/share/games/fortunes/fortunes"))))

(use-package sl
  :ensure t
  :defer t)

(use-package xpm
  :homepage http://www.gnuvola.org/software/xpm/
  :ensure t
  :defer t)

(use-package gnugo                      ; 围棋
  :ensure t
  :disabled t)

(use-package chess                      ; 国际象棋
  :ensure t
  :disabled t)

(use-package pacmacs
  :disabled                      ; It defines `plist-map' without package prefix
  :about "Pac-Man Game"
  :homepage https://github.com/codingteam/pacmacs.el
  :ensure t)

(use-package spinner
  :about "Add spinners and progress-bars to the mode-line for ongoing operations"
  :ensure t
  :defer t)

;; `pulse.el' has the similiar function
(use-package beacon
  :about "Highlight the cursor whenever the window scrolls"
  :ensure t
  :defer t)

(use-package xkcd
  :ensure t
  :defer t)

(use-package lol-data-dragon
  :about "显示 LoL 数据"
  :load-path "~/src/lol-data-dragon.el"
  :commands (lol-data-dragon-champion-image-dired))

(defun chunyang-birthday-p ()
  "Return t if today is my birthday, i.e., 农历九月廿三."
  ;; Adapted from `calendar-chinese-date-string'
  (require 'cal-china)
  (pcase-exhaustive (calendar-chinese-from-absolute
                     (calendar-absolute-from-gregorian
                      (calendar-current-date)))
    (`(,_ ,_ ,m ,d)
     ;; Note: For leap months M is a float
     (equal (list (floor m) d) '(9 23)))))

(defun chunyang-happy-birthday ()
  ;; Avoid slowing down Emacs startup
  (run-with-idle-timer
   1
   nil
   (lambda ()
     (when (chunyang-birthday-p)
       (let ((cursor-type nil))
         (animate-birthday-present user-full-name))))))

(add-hook 'emacs-startup-hook #'chunyang-happy-birthday)


;;; Task

(use-package taskpaper-mode
  :homepage https://github.com/saf-dmitry/taskpaper-mode
  :ensure t
  :defer t
  :config
  (setq taskpaper-tag-alist
        '(("today"       . ?t)
          ("due(%%)"     . ?d)
          ("due(%%+7d)"  . ?s)
          ("priority(1)" . ?1)
          ("priority(2)" . ?2)
          ("priority(3)" . ?3)))
  (defun chunyang-taskpaper-mode-setup ()
    (setq tab-width 4))
  (add-hook 'taskpaper-mode-hook #'chunyang-taskpaper-mode-setup))


;;; Utilities

(use-package popup
  :about "Visual Popup Interface Library (using overlay)"
  :homepage https://github.com/auto-complete/popup-el
  :ensure t
  :defer t)

(use-package quick-peek
  :about "Inline pop-up library (using overlay)"
  :homepage https://github.com/cpitclaudel/quick-peek
  :ensure t
  :commands (quick-peek-show quick-peek-hide))

(use-package pos-tip
  :about "Like tooltip-show but can show at arbitrary position"
  :homepage https://github.com/pitkali/pos-tip
  :ensure t
  :commands pos-tip-show)

(use-package posframe
  :if (version<= "26.1" emacs-version)
  :about "Show a child frame at point"
  :homepage https://github.com/tumashu/posframe
  :ensure t
  :defer t)

(use-package cycle-quotes
  :ensure t
  :defer t)

(use-package helm-unicode
  :about 标点符号等输入
  :ensure t
  :defer t)

(use-package restclient
  :about "Test HTTP API"
  :ensure t
  :defer t)

(use-package verb
  :about Organize and send HTTP requests
  :homepage https://github.com/federicotdn/verb
  :ensure t
  :defer t)

(use-package hexl
  :about (info "(emacs) Editing Binary Files")
  :notes
  - od
  - hexdump
  - xxd
  :commands (hexl-find-file hexl-mode))

(use-package nhexl-mode
  :ensure t
  :notes "Unlike `hexl-mode', this is a minor mode"
  :defer t)

(use-package el2markdown
  :about Convert Emacs Lisp Commentry section into Markdown
  :ensure t
  :defer t)

(use-package ip2region
  :about "IP 地址定位"
  :if module-file-suffix
  :load-path "~/src/emacs-ip2region"
  :commands ip2region)

(use-package cmark
  :about "Markdown parser"
  :if module-file-suffix
  :load-path "~/src/emacs-cmark"
  :commands cmark-markdown-to-html)

(use-package epkg
  :about "Browse the Emacsmirror package database"
  :info (info "(epkg) Top")
  :notes M-x epkg-describe-package is very impressive
  :preface
  (autoload 'epkg-read-package "epkg")
  (defun chunyang-use-package (package)
    (interactive
     (list
      (epkg-read-package
       "Insert use-pacakge form for package: ")))
    (pp
     `(use-package ,(intern package)
        :homepage ,(oref (epkg package) homepage)
        :summary ,(oref (epkg package) summary))
     (current-buffer)))
  :ensure t
  :defer t)

(use-package esup
  :about "the Emacs StartUp Profiler"
  :ensure t
  :defer t)

(use-package lsp-mode
  :disabled t
  :ensure t
  :about "Minor mode for Language Server Protocol"
  :homepage https://github.com/emacs-lsp/lsp-mode
  :notes
  - https://github.com/Microsoft/language-server-protocol/
  - http://langserver.org/
  :defer t
  :config
  ;; For debug gopls
  ;; (setq lsp-gopls-server-args
  ;;       '("-logfile" "/tmp/lsp-gopls.log" "-rpc.trace"))
  ;; XXX The face `lsp-face-highlight-textual' (background yellow) is ugly
  (setq lsp-highlight-symbol-at-point nil))

(use-package eglot
  :about A client for Language Server Protocol servers
  :homepage https://github.com/joaotavora/eglot
  :ensure t
  :commands eglot
  :defer t
  :config

  ;; It is better to put this in .dir-locals.el or use
  ;; `dir-locals-set-class-variables', see (info "(emacs) Directory Variables")
  ;; but let's keep it as simple since for now the only use of eglot is with
  ;; gopls
  ;;
  ;; use plist not alist, see
  ;; https://github.com/joaotavora/eglot/issues/332#issuecomment-547866307
  (setq-default eglot-workspace-configuration
                '((:gopls . (:usePlaceholders t))))

  ;; IDEA: Use `avy' (like `ace-link'
  (bind-key "M-n" #'chunyang-eglot-next-highlight eglot-mode-map)
  (bind-key "M-p" #'chunyang-eglot-previous-highlight eglot-mode-map)

  (defun chunyang-eglot-next-highlight (&optional N)
    (interactive "p")
    (or N (setq N 1))
    (let ((o (car (seq-intersection (overlays-at (point)) eglot--highlights))))
      (cond ((not o) (user-error "No highlight at point"))
            ((= (length eglot--highlights) 1))
            (t (let* ((i (mod (+ N (seq-position eglot--highlights o #'eq))
                              (length eglot--highlights)))
                      (no (nth i eglot--highlights))
                      (offset (- (point) (overlay-start o))))
                 (goto-char (+ (overlay-start no) offset)))))))

  (defun  chunyang-eglot-previous-highlight (&optional N)
    (interactive "p")
    (or N (setq N 1))
    (chunyang-eglot-next-highlight (- N)))

  ;; IDEA: 了解一下 project-level workflow ?
  (defun chunyang-helm-eglot-shutdown ()
    "Shutdown eglot servers.

Unlike `eglot-shutdown', you can shutdown more than one servers
at a time."
    (interactive)
    (let ((servers (cl-loop for servers
                            being hash-values of eglot--servers-by-project
                            append servers))
          (name (lambda (srv)
                  (format "%-20s %s    %s"
                          (eglot--project-nickname srv)
                          (eglot--major-mode srv)
                          (propertize
                           (string-join (mapcar #'abbreviate-file-name (project-roots (eglot--project srv))) ", ")
                           'face 'helm-buffer-process)))))
      ;; Put (eglot-current-server) at the beginning
      (when-let ((current-server (eglot-current-server)))
        (setq servers (cons current-server (delete current-server servers))))
      (helm :sources
            (helm-build-sync-source "Eglot Servers"
              :candidates (seq-mapn #'cons (mapcar name servers) servers)
              :action (helm-make-actions
                       "Delete server(s)"
                       (lambda (_candidate)
                         (mapc #'eglot-shutdown (helm-marked-candidates)))))
            :buffer "*helm eglot servers*")))

  ;; * Elixir
  ;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196
  (add-to-list
   'eglot-server-programs
   '(elixir-mode "~/src/elixir-ls/release/language_server.sh"))

  (defun chunyang-elixir-current-project (dir)
    "Return the current project as a cons cell usable by project.el."
    (let ((project-dir (locate-dominating-file dir "mix.exs")))
      (if project-dir
          (cons 'elixir project-dir)
        nil)))

  (add-hook 'project-find-functions #'chunyang-elixir-current-project)

  (cl-defmethod project-roots ((project (head elixir)))
    (list (cdr project))))

(use-package cquery
  :disabled "Just give it a try"
  :homepage https://github.com/jacobdufault/cquery
  :load-path "~/src/cquery/emacs/"
  :commands lsp-cquery-enable
  :init
  (add-hook 'c-mode-hook #'lsp-cquery-enable)
  (add-hook 'c++-mode-hook #'lsp-cquery-enable)
  (add-hook 'objc-mode-hook #'lsp-cquery-enable))

(use-package transmission
  :ensure t
  :defer t)

(use-package csv-mode
  :about "Major mode for csv files"
  :ensure t
  :defer t
  :hook (csv-mode . hl-line-mode))

(use-package po-mode
  :disabled t                           ; Melpa stopped ship it
  :about "Major mode for PO files"
  :ensure t
  :defer t)

(use-package basic-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package gist
  :disabled t                        ; 依赖 gh.el 的 autoload 加载太慢
  :ensure t
  :homepage https://github.com/defunkt/gist.el
  :defer t)

;; XXX Not working for multiple bytes ?
;; [[https://github.com/magit/ghub/issues/12][Error: Multibyte text in HTTP request · Issue #12 · magit/ghub]]
(use-package yagist
  :ensure t
  :load-path "~/src/yagist.el"
  :commands (yagist-buffer yagist-buffer-private)
  :homepage https://github.com/mhayashi1120/yagist.el
  :defer t)

(use-package fix-anything
  :commands fix-anything)

(use-package countdown
  :load-path "~/src/countdown.el"
  :commands countdown)

(use-package unifont
  :load-path "~/src/unifont.el"
  :commands (unifont-test unifont-insert))

(use-package cli-docs
  :load-path "~/src/cli-docs.el"
  :about 命令行工具文档 (中文)
  :commands (cli-docs cli-docs-helm))

(use-package tldr
  :homepage https://github.com/kuanyui/tldr.el
  :ensure t
  :defer t)

(use-package tldr2
  :commands tldr2-find-file)

(use-package sexp-diff
  :load-path "~/src/sexp-diff.el"
  :defer t)

(use-package geoip
  :load-path "~/src/geoip.el"
  :defer t)

;; http://jodies.de/ipcalc
;; https://www.deepspace6.net/projects/ipv6calc.html
(use-package ipcalc
  :ensure t
  :commands ipcalc)


;;; Other Emacs Lisp libraries

;; dash, seq, thunk, stream, ewoc, cl-lib, widget, tree-widget, let-alist, map
;; ring, radix-tree, dict-tree, trie, heap, avl-tree

(use-package parsec
  :about "A parser combinator library"
  :ensure t
  :defer t)

(use-package hierarchy
  :homepage https://github.com/DamienCassou/hierarchy
  :about "Library to create and display hierarchy structures"
  :ensure t
  :defer t)

(use-package ctable
  :about "Library for creating tables"
  :homepage https://github.com/kiwanami/emacs-ctable
  :ensure t
  :defer t)

(use-package etable
  :about "Library for creating tables."
  :homepage https://github.com/Fuco1/ETable
  :ensure t
  :defer t)

(use-package esxml
  :homepage https://github.com/tali713/esxml
  :about "XML writing and Document.querySelector()"
  :ensure t
  :defer t)

(use-package xmlgen
  :about "XML writing"
  :notes "Unlike esxml, xmlgen uses its own sexp syntax and don't with work libxml-parse-html-region"
  :ensure t
  :defer t)

(use-package chart                      ; Built-in
  :defer t
  :defer t)

(use-package bui
  :homepage https://github.com/alezost/bui.el
  :ensure t
  :defer t)

(use-package trie
  :info https://en.wikipedia.org/wiki/Trie
  :ensure t
  :defer t)

(use-package strie
  :homepage https://github.com/jcatw/strie.el
  :ensure t
  :defer t)

(use-package request
  :ensure t
  :homepage https://github.com/tkf/emacs-request
  :defer t)

(use-package aio
  :ensure t
  :about skeeto/emacs-aio: async/await for Emacs Lisp
  :homepage https://github.com/skeeto/emacs-aio
  :defer t)

(use-package commander
  :about Emacs command line parser
  :homepage http://github.com/rejeep/commander.el
  :ensure t
  :defer t)

(use-package msgpack
  :load-path "~/src/msgpack.el"
  :defer t)


;;; Debugger

(use-package gud
  :info (info "(emacs) Debuggers")
  :defer
  :config
  (setq gud-pdb-command-name "python -m pdb")
  ;; `pdb'
  (define-advice pdb (:after (&rest _) fix-gud-statement)
    (gud-def gud-statement "!%e"      "\C-e" "Execute Python statement at point."))

  (defhydra hydra-pdb (:hint nil :foreign-keys run)
    "
^Running^         ^Breakpoints^   ^Data^          ^Frame
^^^^^^^^-----------------------------------------------------
_n_: next         _b_: set        _p_: print exp  _u_: up
_s_: step         _r_: remove     ^ ^             _d_: down
_c_: continue
_r_: return
"
    ;; Running
    ("n" gud-next)
    ("s" gud-step)
    ("c" gud-cont)
    ("r" gud-finish)
    ;; Breakpoints
    ("b" gud-break)
    ("r" gud-remove)
    ;; Data
    ("p" gud-print)
    ("e" gud-statement)
    ;; Frame
    ("u" gud-up)
    ("d" gud-down)
    ;; Quit hydra
    ("q" nil "quit" :color blue)))

(use-package realgud
  :disabled t                           ; Lots of byte compiling warnings
  :homepage https://github.com/realgud/realgud
  :ensure t
  :defer t
  :config
  (setq realgud-safe-mode nil)
  (setq realgud:pdb-command-name "python -m pdb"))


;;; Custom

;; All right, enough is enough, ALL themes are safe to me.
(setq custom-safe-themes t)
;; (load custom-file :no-error :no-message)

;;; init.el ends here

;; Local Variables:
;; bug-reference-url-format: "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
;; bug-reference-bug-regexp: "bug#\\(?2:[0-9]+\\)"
;; eval: (bug-reference-prog-mode)
;; End:
