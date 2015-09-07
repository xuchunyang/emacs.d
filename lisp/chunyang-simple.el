;;; chunyang-simple.el --- Simple editing functions  -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)


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

(defun chunyang-other-window (arg)
  "Run `other-window'.
With ARG, swap two window."
  (interactive "P")
  (if arg
      (chunyang--swap-window)
    (other-window +1)))


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

(defalias 'demo #'chunyang-make-another-scratch-buffer)


;;; Download stuffs

(defun chunyang-string-url-p (string)
  "Test STRING is url. "
  (with-temp-buffer
    (insert string)
    (thing-at-point 'url)))

(defun chunyang-download-file (url file)
  "Download URL as FILE."
  (interactive
   (let* ((default-url (or (gui-selection-value) (car kill-ring)))
          (prompt (if (and default-url
                           (chunyang-string-url-p default-url))
                      (format "URL (\"%s\"): " default-url)
                    "Repo URL: "))
          (url (read-string prompt nil nil default-url))
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


;;; Opening iTerm

(defun project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))

;; with args, go to project root
(defun iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
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
  " cmd))))


(defun chunyang-copy-buffer-name-as-kill (buf)
  "Put name of the current buffer to kill-ring."
  (interactive "b")
  (kill-new buf))


;;; Restore the *scratch* buffer
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

(defun chunyang-switch-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
