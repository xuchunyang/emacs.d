;;; chunyang-simple.el --- Simple editing functions  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)


;;;###autoload
(defun chunyang-split-window-right ()
  "Split window with another buffer."
  (interactive)
  (select-window (split-window-right))
  (switch-to-buffer (other-buffer)))

(defun chunyang-split-window-below ()
  "Split window with another buffer."
  (interactive)
  (select-window (split-window-below))
  (switch-to-buffer (other-buffer)))

(defun chunyang--swap-window ()
  (let ((this-buffer (window-buffer (selected-window)))
        (other-buffer (prog2
                          (other-window +1)
                          (window-buffer (selected-window))
                        (other-window -1))))
    (switch-to-buffer other-buffer)
    (switch-to-buffer-other-window this-buffer)))

;;;###autoload
(defun chunyang-other-window (arg)
  "Run `other-window'.
With ARG, swap two window."
  (interactive "P")
  (if arg
      (chunyang--swap-window)
    (other-window +1)))

(defun chunyang-format-time (time)
  "Convert 'October 20, 2014' to '2014-10'."
  (format-time-string
   "%Y-%m"
   (apply 'encode-time (parse-time-string
                        (concat time ", 12:12:12")))))

;;;###autoload
(defun chunyang-insert-current-date (arg)
  "Display current date.
With prefix argument, insert current date at point."
  (interactive "P")
  (funcall (if arg 'insert 'message) (format-time-string "%Y/%m/%d")))

(defun chunyang-region-length (beg end)
  "Return the length of current region."
  (interactive "r")
  (message "[%d]" (- end beg)))

(defun chunyang-count-lines-region (beg end)
  "Count the number of lines in the region."
  (interactive "r")
  (message "%d" (count-lines beg end)))

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;; \\(.+\\)$" 1) t))

;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(defun chunyang-mail-can-use-osx-open ()
  "Return non-nil if the OS X \"open\" command is available for mailing."
  (and (featurep 'ns)
       (equal (executable-find "open") "/usr/bin/open")
       (memq system-type '(darwin))))

;;;###autoload
(defun chunyang-mail-insert-to-mailer ()
  "Send the message to your preferred mail client.
This requires either the OS X \"open\" command, or the freedesktop
\"xdg-email\" command to be available."
  (interactive)
  (save-excursion
    ;; FIXME? use mail-fetch-field?
    (let* ((to (progn
		 (goto-char (point-min))
		 ;; (forward-line)
		 (and (looking-at "^To: \\(.*\\)")
		      (match-string-no-properties 1))))
	   (subject (progn
		      (forward-line)
		      (and (looking-at "^Subject: \\(.*\\)")
			   (match-string-no-properties 1))))
	   (body (progn
		   (forward-line 2)
		   (if (> (point-max) (point))
		       (buffer-substring-no-properties (point) (point-max))))))
      (if (and to subject body)
	  (if (chunyang-mail-can-use-osx-open)
	      (start-process "/usr/bin/open" nil "open"
			     (concat "mailto:" to
				     "?subject=" (url-hexify-string subject)
				     "&body=" (url-hexify-string body)))
	    (start-process "xdg-email" nil "xdg-email"
			   "--subject" subject
			   "--body" body
			   (concat "mailto:" to)))
        (error "Subject, To or body not found")))))

(defun chunyang--notmuch-unread-count ()
  "Number of unread mails by notmuch."
  (string-to-number
   (shell-command-to-string
    "notmuch search tag:unread | wc -l")))


;;;###autoload
(defun chunyang-make-another-scratch-buffer (arg)
  "Make another *scratch* buffer.
With ARG, put *scratch* buffer right."
  (interactive "P")
  (let ((buf (get-buffer-create "*scratch*")))
    (when (zerop (buffer-size buf))
      (with-current-buffer buf
        (insert initial-scratch-message)
        (lisp-interaction-mode)))
    (when arg
      (split-window-right)
      (other-window 1))
    (switch-to-buffer buf)))
;;;###autoload
(defalias 'demo #'chunyang-make-another-scratch-buffer)


;;; stole from https://github.com/expez/.emacs.d/blob/master/lisp/init-util.el
(defun chunyang-launch (command)
  "Launch an application from Emacs, with its own output
buffer. This is like asynch-shell-command but allows for any
number of processes at a time, rather than just one. If given a
prefix argument, the process's buffer is displayed."
  (interactive (list (read-shell-command (concat default-directory "$ "))))
  (let* ((name (car (split-string-and-unquote command)))
         (buffer (generate-new-buffer (concat "*" name "*"))))
    (set-process-sentinel (start-process-shell-command name buffer command)
                          'chunyang-launch-sentinel)
    (if (eq (car current-prefix-arg) 4)
        (display-buffer buffer))))

(defun chunyang-launch-sentinel (proc event)
  "Reports on changes in `chunyang-launch'ed applications."
  (message (format "%s: %s" proc event)))


;;; TODO: finish this.
(defun chunyang-kill-all-buffer ()
  "Kill almost all buffers."
  (interactive)
  (mapc (lambda (elt)
          (with-current-buffer elt
            (save-buffer elt)))
        (seq-remove
         (lambda (elt)
           (member (buffer-name elt)
                   '("*scratch*" "*Messages*" "*Help*" "*info*")))
         (buffer-list))))


;;; Download stuffs
(defun chunyang-download-file (url file)
  "Download URL as FILE."
  (interactive
   (let* ((url (read-string "URL: "))
          (guess (file-name-nondirectory url))
          (default-file (concat default-directory guess))
          (file (read-file-name
                 "Save to: " nil nil nil default-file)))
     (list url file)))
  (unless (require 'spinner nil t)
    (error "Package `spinner' not found."))
  (unless (alist-get 'download spinner-types)
    (push '(download . ["下" "载" "中"]) spinner-types))
  (let* ((spinner-stop-func (spinner-start 'download 3))
         (output-buffer "*curl-output*")
         (proc (start-process "curl" output-buffer
                              "curl" url "-o" file)))
    (set-process-sentinel
     proc (lambda (proced event)
            (unwind-protect
                (if (string-equal event "finished\n")
                    (progn
                      (message "Download as \"%s\" done." file)
                      (kill-buffer output-buffer))
                  (error "Download failed, see %s buffer for details" output-buffer))
              (funcall spinner-stop-func))))))

(defvar chunyang-git-clone-done-action #'dired
  "Funcall called by `chunyang-git-clone'.")

(defvar chunyang-git--spinner-stop nil
  "Holds the function that stops the spinner.")

(defun chunyang-git-clone (repo dir)
  "Git clone asynchronously."
  (interactive
   (let* ((default (or (gui-selection-value) (car kill-ring)))
          (prompt (if default (format "Repo URL (\"%s\"): " default)
                    "Repo URL: "))
          (repo (read-string prompt nil nil default))
          (guess (file-name-base repo))
          (dir (read-directory-name
                (format "Clone %s to: " repo)
                nil nil nil guess)))
     (list repo dir)))
  (setq chunyang-git--spinner-stop (spinner-start))
  (let* ((command (format "git clone %s %s" repo dir))
         (output-buffer "*git-clone-output*")
         (proc (start-process-shell-command "git-clone" output-buffer command)))
    (set-process-sentinel
     proc
     (lambda (process event)
       (unwind-protect
           (if (string-equal event "finished\n")
               (progn
                 (kill-buffer output-buffer)
                 (message "Git clone done.")
                 (when (fboundp chunyang-git-clone-done-action)
                   (funcall chunyang-git-clone-done-action dir)))
             (error "Git clone failed, see %s buffer for details" output-buffer))
         ;; Stop spinner no matter successes or fails
         (when (functionp chunyang-git--spinner-stop)
           (funcall chunyang-git--spinner-stop)
           (setq chunyang-git--spinner-stop nil)))))))


;;; Misc
(defun chunyang-sum-numbers-in-region (beg end)
  "Sum numbers in region.  Notes the effect with `string-to-number'.

See also Emacs SE question: URL
  `http://emacs.stackexchange.com/questions/10939/sum-numbers-in-region'."
  (interactive "r")
  (message
   "%s"
   (seq-reduce
    #'+
    (mapcar #'string-to-number (split-string (buffer-substring beg end)))
    0)))

;; Provide a keybinding to rapidly search through all files an entire git repo
(defun git-grep-repo (search)
  "git-grep the entire current repo"
  (interactive
   (progn
     (require 'grep)
     (list (grep-read-regexp))))
  (let ((command (grep-expand-template
                  (concat "git --no-pager grep --no-color -n -e <R> "
                          "`git rev-parse --show-toplevel`") search)))
    (grep-find command)))
(global-set-key (kbd "C-x v f") 'git-grep-repo)


;;; Debug and Log
(defvar chunyang-debug-buffer "*Debug Chunyang Log*")

(defvar chunyang-debug nil
  "If non-nil, write log message into `chunyang-debug-buffer' buffer.
It is disabled by default because `chunyang-debug-buffer' grows quickly.")

;; Utility: logging
(defun chunyang-log (format-string &rest args)
  "Log message `chunyang-debug' is non-nil.
Messages are written to the `chunyang-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when chunyang-debug
    (with-current-buffer (get-buffer-create chunyang-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format (concat (if (string-match "Start session" format-string)
                                    "* " "** ")
                                "%s.%06d (%s)\n %s\n")
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (chunyang-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defun chunyang-log-get-current-function ()
  "Get function name calling `chunyang-log'.
The original idea is from `tramp-debug-message'."
  (cl-loop with exclude-func-re = "^chunyang-\\(?:interpret\\|log\\|.*funcall\\)"
           for btn from 1 to 40
           for btf = (cl-second (backtrace-frame btn))
           for fn  = (if (symbolp btf) (symbol-name btf) "")
           if (and (string-match "^chunyang" fn)
                   (not (string-match exclude-func-re fn)))
           return fn))

(defun chunyang-set-variable ()
  (interactive)
  (eval-minibuffer
   "Eval: " (let ((var (variable-at-point)))
              (when (boundp var)
                (format "(setq %s nil)" var)))))


;;; Key binding
;;
;;

;;; `forward-line'
;; (bind-key "C-S-n" #'chunyang-line-adjust)
(defun chunyang-line-adjust (inc)
  "Move cursor vertically down/up INC lines."
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              (?n inc)
              (?p (- inc))
              (t inc))))
      (forward-line step)
      (message "Use n,p for further move lines.")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?n ?p))
             (define-key map (vector (append mods (list key)))
               (lambda ()               ; `lexical-binding' must be t
                 (interactive)
                 (chunyang-line-adjust (abs inc)))
               )))
         map)))))

;;; C-x [, C-x ]
;; (bind-key [remap backward-page] #'chunyang-page-adjust)
;; (bind-key [remap forward-page]  #'chunyang-page-adjust)
(defun chunyang-page-adjust (inc)
  ""
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              (?\[ (- inc))
              (?\] inc)
              (t  inc))))
      (forward-page step)
      (message "Use [,] for further move.")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(91 93))
             (define-key map (vector (append mods (list key)))
               (lambda () (interactive) (chunyang-page-adjust (abs inc))))))
         map)))))


;;; MAc OS X

(defun chunyang-run-command-in-iterm (command &optional arg)
  "Run command in Iterm.  With ARG, change to `default-directory' firstly."
  (interactive (list
                (read-shell-command
                 "Run command in ITerm: "
                 (when (use-region-p)
                   (buffer-substring
                    (region-beginning) (region-end))))
                current-prefix-arg))
  (do-applescript
   (format
    "
  tell application \"iTerm\"
       activate
       set _session to current session of current terminal
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  "
    (if (and arg default-directory)
        (format "cd %s && %s" default-directory command)
      command))))


;;; 用于回复水木上的帖子哦
(defun chunyang-reply-smth ()
  (interactive)
  (with-current-buffer (get-buffer-create (make-temp-name "smth-"))
    (switch-to-buffer (current-buffer))
    (yank)
    (unless (fboundp 'chunyang--add-prefix)
      (fset 'chunyang--add-prefix
            (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217788 24 32 134217790 1 24 114 116 58 32 return] 0 "%d")) arg))))
    (chunyang--add-prefix)
    (auto-fill-mode)))


;;;###autoload
(defun chunyang-copy-buffer-name-as-kill (buf)
  "Put name of the current buffer to kill-ring."
  (interactive "b")
  (kill-new buf))


(defun chunyang--new-mail-notify ()
  (let (new (string-to-number
             (shell-command-to-string
              "find Maildir -mmin -3 -a -type f | wc -l")))
    (if (> new 0)
        (shell-command "echo -n \"red\" | nc -4u -w0 localhost 1738")
      (shell-command "echo -n \"black\" | nc -4u -w0 localhost 1738"))))


;;; Scratch buffer
(defvar chunyang-scratch-log-file (expand-file-name
                                   ".scratch.bak~" user-emacs-directory))

(defun chunyang-save-scratch ()
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (widen)
        (ignore-errors
          (write-region (point-min) (point-max) chunyang-scratch-log-file))))))

(defun chunyang-restore-scratch ()
  (interactive)
  (let ((buf (get-buffer "*scratch*")))
    (when buf
      (with-current-buffer buf
        (erase-buffer)
        (insert-file-contents chunyang-scratch-log-file)))))


;;; Package
(defun helm-show-selected-packages ()
  (interactive)
  (helm :sources (helm-build-sync-source "Selected Packages"
                   :candidates package-selected-packages
                   :coerce #'intern
                   :action
                   (helm-make-actions
                    "Describe package"
                    (lambda (candidate)
                      (describe-package candidate))
                    "Visit homepage"
                    (lambda (candidate)
                      (let* ((pkg candidate)
                             (desc (cadr (assoc pkg package-archive-contents)))
                             (extras (package-desc-extras desc))
                             (url (and (listp extras) (cdr-safe (assoc :url extras)))))
                        (if (stringp url)
                            (browse-url url)
                          (message "Package %s has no homepage"
                                   (propertize (symbol-name pkg)
                                               'face 'font-lock-keyword-face)))))))
        :candidate-number-limit 9999))


(defun chunyang-weibo-post (text)
  (interactive
   (let* ((default (when (use-region-p)
                     (concat
                      (buffer-substring
                       (region-beginning) (region-end))
                      "(Sent from #Emacs#)")))
          (prompt (if default (format "发微博 (\"%s\"): " default)
                    "发微博: "))
          (string (read-string prompt nil nil default)))
     (list string)))
  (require 'weibo)
  (weibo-send-status text))


;;; Eshell (from [[https://www.masteringemacs.org/article/pcomplete-context-sensitive-completion-emacs][PComplete: Context-Sensitive Completion in Emacs - Mastering Emacs]])
;; also see [[https://github.com/emacs-helm/helm/wiki#22-completion-in-emacs-shell][Home · emacs-helm/helm Wiki]]
(defconst pcmpl-git-commands
  '("add" "bisect" "branch" "checkout" "clone"
    "commit" "diff" "fetch" "grep"
    "init" "log" "merge" "mv" "pull" "push" "rebase"
    "reset" "rm" "show" "status" "tag" )
  "List of `git' commands")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE"
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let ((ref-list))
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (add-to-list 'ref-list (match-string 1)))
      ref-list)))

(defun pcomplete/git ()
  "Completion for `git'"
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  ;; complete files/dirs forever if the command is `add' or `rm'
  (cond
   ((pcomplete-match (regexp-opt '("add" "rm")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (pcmpl-git-get-refs "heads")))))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
