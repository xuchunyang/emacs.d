;;; chunyang-misc.el --- Miscellaneous               -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; -----------------------------------------------------------------------------
;; Show recent project/file in a buffer
;;

;; The code requires Projectile and Recentf for their data.  Also, this idea is
;; inspired by Spacemacs's startup buffer, see <http://spacemacs.org/>.


(defun chunyang-insert-file-list (list-display-name list)
  ;; Copied from `spacemacs-buffer//insert-file-list'.
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun chunyang-list-recentf-and-projects ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Recentf & Project List*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (chunyang-insert-file-list "Recent Files:" (recentf-elements 5))
    (insert "\n\n")
    (chunyang-insert-file-list "Projects:" projectile-known-projects)
    (goto-char (point-min))
    (page-break-lines-mode)
    (read-only-mode))
  (local-set-key (kbd "RET") 'widget-button-press)
  (local-set-key [down-mouse-1] 'widget-button-click))

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
