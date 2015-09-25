(defvar annotate-word--overlays-tail nil
  "Hold overlays for tailing word explanations.")

(defun annotate-word--overlay (str pt)
  "Create an overlay with STR at PT."
  (when (<= (1+ pt) (point-max))
    (let* ((ol (make-overlay pt (1+ pt)))
           (old-str (buffer-substring pt (1+ pt))))
      (overlay-put ol 'display (concat str old-str))
      (push ol annotate-word--overlays-tail))))

(defvar annotate-word-alist
  '(("sufficient" . "足够的")
    ("primary" . "初级、主要")
    ("corresponding" . "相应的")))

(defun annotate-word ()
  (interactive)
  (setq prog-mode-p (derived-mode-p 'prog-mode))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\w+" nil t)
      (when (or (not prog-mode-p)
                (and prog-mode-p (nth 4 (syntax-ppss))))
        (let* ((word (match-string 0))
               (pt (match-end 0))
               (annotation (assoc-default word annotate-word-alist
                                          (lambda (s1 s2)
                                            (string= (downcase s1)
                                                     (downcase s2))))))
          (when annotation
            (setq annotation (concat "(" annotation ")"))
            (annotate-word--overlay annotation pt)))))))

(defun annotate-word-clear ()
  (interactive)
  (mapc #'delete-overlay annotate-word--overlays-tail))

(provide 'annotate-word)
