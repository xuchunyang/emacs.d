;;; Make 'C-u C-SPC' always goes to last 'C-SPC C-SPC'

;; TODO (just some ideas:
;; - Verbose with `message'
;; - Visibility with overlay
;;
;; or just use `register' instead

(defvar-local pin-mark nil)

(defun pin-set ()
  (interactive)
  (unless pin-mark
    (setq pin-mark (make-marker)))
  (set-marker pin-mark (point)))

(defun pin-return ()
  (interactive)
  (if pin-mark
      (goto-char pin-mark)
    (user-error "No pin found")))

;; 'C-<SPC> C-<SPC>' to `pin-set'
(add-hook 'deactivate-mark-hook
          (defun pin-set--function ()
            (when (eq last-command 'set-mark-command)
              (pin-set))))

;; 'C-u C-<SPC>' to `pin-return'
(defun set-mark-command--pin-return (orig-func &rest args)
  (apply orig-func args)
  (when (equal current-prefix-arg '(4))
    (pin-return)))

(advice-add 'set-mark-command :around #'set-mark-command--pin-return)

(provide 'pin)
