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


(defun mark-window ()
  "Put mark at end of window, point at beginning."
  (interactive)
  (goto-char (window-end nil :update))
  (push-mark nil t t)
  (goto-char (window-start)))


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

(defun chunyang-switch-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun scratch-clear ()
  "Clear *scratch* buffer."
  (interactive)
  (with-current-buffer "*scratch*"
    (if (string= "*scratch*" (buffer-name))
        (let ((standard-value
               (eval (car (get 'initial-scratch-message 'standard-value)))))
          (erase-buffer)
          (insert (substitute-command-keys standard-value))))))

(defun my-list-setnth (list n new-elt)
  (setcar (nthcdr n list) new-elt))


(defmacro remap-key (from-key-name to-key-name)
  ;; Assuming `global-map' and ignoring prefix arg for simplicity
  (let ((cmd `(lambda (&optional arg)
                (interactive "p")
                (execute-kbd-macro (read-kbd-macro ,from-key-name)))))
    `(define-key (current-global-map) (read-kbd-macro ,to-key-name) ,cmd)))
;; (remap-key "C-e RET" "C-o")

(defmacro define-command-from-key (cmd-name key-name)
  `(defun ,cmd-name (&optional arg)
     (interactive "p")
     (execute-kbd-macro (read-kbd-macro ,key-name))))
;; (define-command-from-key foo "C-e RET")

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
