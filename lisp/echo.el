;;; echo.el --- Echo something about thing at point

;;;###autoload
(define-minor-mode echo-mode
  "Echo something about thing at point like eldoc."
  nil " Echo" nil
  (if echo-mode
      (add-hook 'post-command-hook 'echo-schedule-timer nil t)
    (remove-hook 'post-command-hook 'echo-schedule-timer t)))

(defvar echo-timer nil)
(defvar echo-idle-delay 0.50)
(defvar echo-function #'bing-message-current-word)


(defun echo-message-without-log (format-string &rest args)
  (let ((message-log-max nil))
    (apply #'message format-string args)))

(advice-add 'bing-dict--message :override #'echo-message-without-log)

(defun bing-message-current-word ()
  (let ((word (current-word nil t)))
    (when word
      (bing-dict-brief word))))

(defun echo-schedule-timer ()
  (unless (and echo-timer
               (memq eldoc-timer timer-idle-list)) ;FIXME: Why?
    (setq echo-timer
          (run-with-idle-timer
           echo-idle-delay nil
           (lambda ()
             (when echo-mode
               (echo-message)))))))

(defun echo-message-p ()
  (require 'eldoc)
  (and (eldoc-display-message-no-interference-p)
       (not this-command)))

(defun echo-message ()
  (with-demoted-errors "echo error: %s"
    (when (echo-message-p)
      (funcall echo-function))))

(provide 'echo)
;;; echo.el ends here
