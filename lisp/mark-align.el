(defvar mark-align-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C- ] #'mark-align-set-mark)
    map))

(defun mark-align-set-mark ()
  (interactive)
  )

(define-minor-mode mark-align-mode
  "Align through marking."
  :lighter " Mark-Align"
  (if mark-align-mode
      )
  )
