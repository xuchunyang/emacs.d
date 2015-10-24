;;; helm-init-config.el --- Some helm commands for Emacs init configuration  -*- lexical-binding: t; -*-

(require 'helm)
(require 'chunyang-elisp)               ; for `awhen'

(defun helm-use-package ()              ; See also `helm-imenu-in-all-buffers'
  "Discover use-package config stored in `user-init-file'."
  (interactive)
  (require 'helm-imenu)
  (helm :sources
        (helm-make-source "use-package" 'helm-imenu-source
          :candidates
          (lambda ()
            (let ((user-init-buffer
                   (cl-loop for buffer in (buffer-list)
                            when (and (buffer-file-name buffer)
                                      (string= (buffer-file-name buffer)
                                               user-init-file))
                            return buffer
                            finally return (find-file-noselect user-init-file))))
              (helm-imenu-candidates user-init-buffer)))
          ;; TODO: Show some context, please
          :action '(("Show use-package Config" .
                     (lambda (candidate)
                       (helm-imenu--maybe-switch-to-buffer candidate)
                       (imenu candidate))))
          :persistent-help "Show use-package Config")
        :buffer "*helm use-package*"))

(defun helm-imenu-candidates-in-all-init-buffers ()
  "Adapted from `helm-imenu-candidates-in-all-buffers'."
  (let* ((repo (expand-file-name "~/.emacs.d/"))
         repo-files repo-buffers)
    (let ((default-directory repo))
      (setq repo-files
            (mapcar #'expand-file-name
                    (split-string
                     (shell-command-to-string
                      "git ls-files --full-name --")))))
    (setq repo-buffers
          (cl-remove-if-not
           (lambda (buffer)
             (with-current-buffer buffer
               (and (awhen (buffer-file-name)
                      (member it repo-files))

                    (string-match (concat "^" (expand-file-name "~/.emacs.d/"))
                                  default-directory))))
           (buffer-list)))
    (cl-loop for b in repo-buffers
             for count from 1
             for mm = (with-current-buffer b major-mode)
             for cmm = (with-helm-current-buffer major-mode)
             when (or (with-helm-current-buffer
                        (derived-mode-p mm))
                      (with-current-buffer b
                        (derived-mode-p cmm)))
             append (with-current-buffer b
                      (helm-imenu-candidates b)))))

(defvar helm-source-imenu-init nil)

(defun helm-imenu-in-init-buffers ()
  (interactive)
  (unless helm-source-imenu-init
    (setq helm-source-imenu-init
          (helm-make-source "Imenu in all init buffers" 'helm-imenu-source
            :candidates 'helm-imenu-candidates-in-all-init-buffers)))
  (let ((imenu-auto-rescan t)
        (str (thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'helm-source-imenu-init
          :default (list (concat "\\_<" str "\\_>") str)
          :preselect (unless (memq 'helm-source-imenu-init
                                   helm-sources-using-default-as-input)
                       str)
          :buffer "*helm imenu init*")))

(provide 'helm-init-config)
;;; helm-init-config.el ends here
