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

(defun chunyang-window-click-swap (event)
  "Click a window/buffer to swap."
  (interactive "kClick a window to swap: ")
  (let ((buf0 (current-buffer))
        (win0 (get-buffer-window)))
    (let* ((pos (nth 1 (aref event 0)))
           (win1 (posn-window pos))
           (buf1 (window-buffer win1)))
      (if (equal win0 win1)
          (user-error "Two windows are the same, can't swap")
        (message "Swapping %s and %s..." buf0 buf1)
        (select-window win0)
        (switch-to-buffer buf1)
        (select-window win1)
        (switch-to-buffer buf0)
        (message "Swapping %s and %s...Done" buf0 buf1)))))


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

(defun chunyang-list-equals (list pred)
  "Return t if all elements in LIST is equal, test using PRED.
If LIST is empty or has only one element, return t."
  (cl-loop for x on list
           for a = (car x)
           when (cdr x)
           unless (funcall pred a (car it)) return nil
           finally return t))

(defun key-equal-p (key1 key2)
  (when (stringp key1)
    (setq key1 (read-kbd-macro key1 t)))
  (when (stringp key2)
    (setq key2 (read-kbd-macro key2 t)))
  (cl-assert (vectorp key1))
  (cl-assert (vectorp key2))
  (equal key1 key2))

(defun keys-equal-p (&rest keys)
  (chunyang-list-equals keys 'key-equal-p))


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



(defun chunyang-show-number-as-char (undo)
  "Show number in the region as character, for example, C-x for 24.
If no usable region, the whole current buffer will be used.

I find the output of 'C-h v help-map' is hard to read."
  (interactive "P")
  (if undo
      (remove-overlays nil nil 'chunyang-show-number-as-char t)
    (seq-let (beg end) (if (use-region-p)
                           (list (region-beginning) (region-end))
                         (list (point-min) (point-max)))
      (save-excursion
        (goto-char beg)
        (let ((count 0)
              ov)
          (while (re-search-forward "[0-9]+" end :no-error)
            (setq ov (make-overlay (match-beginning 0) (match-end 0)))
            (overlay-put ov 'display (single-key-description
                                      (string-to-number (match-string 0))))
            (overlay-put ov 'chunyang-show-number-as-char t)))))))


;;; Pairs

;; When `electric-pair-mode' doesn't work, I don't really understand
;; the mode and are not able to custom it to do what I want.
;;
;; One can also use C-M-% and back-reference \&, see (info "(emacs) Regexp Replace")
(defun chunyang-insert-pairs-on-region (beg end left-pair right-pair)
  (interactive
   (let ((beg (region-beginning))
         (end (region-end))
         (l (read-string "Left Pair: "))
         (r (read-string "Right Pair: ")))
     (list beg end l r)))
  (insert left-pair (delete-and-extract-region beg end) right-pair))


;;; Filename

(declare-function helm-ffap-guesser "helm-files" ())

(defun chunyang-cycle-filename ()
  "Cycle filename at point between absolute, abbreviated and relative path.

Absolute:     /Users/xcy/.emacs.d/init.el
Abbreviated:  ~/.emacs.d/init.el
Relative:     ../init.el
"
  (interactive "*")
  (require 'helm-files)
  ;; XXX: Since `ffap-guesser' not work well
  (let ((filename (helm-ffap-guesser)))
    (if filename
        (let* ((beg)
               (end)
               (replace
                (lambda (new-string)
                  (delete-region beg end)
                  (insert new-string))))
          (save-excursion
            (goto-char (line-beginning-position))
            (search-forward filename)
            (setq beg (match-beginning 0)
                  end (match-end 0)))
          (cond ((string-prefix-p "/" filename)
                 (funcall replace (abbreviate-file-name filename)))
                ((string-prefix-p "~/" filename)
                 (funcall replace (file-relative-name filename)))
                (t
                 (funcall replace (expand-file-name filename)))))
      (user-error "No filename found at point"))))


;;; Sorting

(defun chunyang-sort-sexps (reverse beg end)
  "Sort sexps in the Region."
  (interactive "*P\nr")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let ((nextrecfun (lambda () (skip-syntax-forward "-.>")))
          (endrecfun  #'forward-sexp))
      ;; To ignore case, change `sort-fold-case'´to non-nil
      (sort-subr reverse nextrecfun endrecfun))))


;;; Defer M-x

(defvar chunyang-defer-M-x-cmd nil)

(defun chunyang-defer-M-x-run ()
  ;; (message "=> %s %s" this-command this-original-command)
  (unless (eq this-command 'chunyang-defer-M-x)
    ;; XXX Prefix argument
    (command-execute chunyang-defer-M-x-cmd)
    (setq chunyang-defer-M-x-cmd nil)
    (remove-hook 'post-command-hook #'chunyang-defer-M-x-run)))

(defun chunyang-defer-M-x (command)
  (interactive "Cdefer M-x ")
  (setq chunyang-defer-M-x-cmd command)
  (add-hook 'post-command-hook #'chunyang-defer-M-x-run))


;; Or simply use M-x el-search-pattern RET (top-level) RET
(defun chunyang-count-top-level-expression ()
  "Count toplevel expressions."
  (interactive)
  (save-excursion
    (let ((count 0) op)
      (goto-char (point-min))
      (while (setq op (scan-sexps (point) 1))
        (goto-char op)
        (cl-incf count))
      (message "Buffer has %s expressions" count)
      count)))

(provide 'chunyang-simple)
;;; chunyang-simple.el ends here
