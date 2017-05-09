;;; chunyang-shell.el --- Shell support              -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs should load this file for emacsclient(1)

;;; Code:

;; Copied from `eshell/info'
;; for ~/.emacs.d/misc/emacs.sh
(defun shell/info (&rest args)
  "Run the info command in-frame with the same behavior as command-line `info', ie:
  `info'           => goes to top info window
  `info arg1'      => IF arg1 is a file, then visits arg1
  `info arg1'      => OTHERWISE goes to top info window and then menu item arg1
  `info arg1 arg2' => does action for arg1 (either visit-file or menu-item) and then menu item arg2
  etc."
  (eval-and-compile (require 'info))
  (let ((file (cond
               ((not (stringp (car args)))
                nil)
               ((file-exists-p (expand-file-name (car args)))
                (expand-file-name (car args)))
               ((file-exists-p (concat (expand-file-name (car args)) ".info"))
                (concat (expand-file-name (car args)) ".info")))))

    ;; If the first arg is a file, then go to that file's Top node
    ;; Otherwise, go to the global directory
    (if file
        (progn
          (setq args (cdr args))
          (Info-find-node file "Top"))
      (Info-directory))

    ;; Treat all remaining args as menu references
    (while args
      (Info-menu (car args))
      (setq args (cdr args)))))

(provide 'chunyang-shell)
;;; chunyang-shell.el ends here
