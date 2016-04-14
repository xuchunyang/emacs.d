;; ace-link support
(defun ace-link--notmuch-hello-collect ()
  "Collect the positions of visible links in *notmuch-hello*."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (- (window-end) 57))
        (goto-char (point-min))
        (forward-line 3)
        (setq pt (point))
        (while (progn (widget-forward 1)
                      (> (point) pt))
          (setq pt (point))
          (when (get-char-property (point) 'button)
            (push (point) candidates)))))
    (nreverse candidates)))

(defun ace-link--notmuch-hello-action (pt)
  (when (number-or-marker-p pt)
    (goto-char (1+ pt))
    (widget-button-press (point))))

(defun ace-link-notmuch-hello ()
  "Open a visible link in *notmuch-hello*."
  (interactive)
  (let ((pt (avy-with ace-link-notmuch-hello
              (avy--process
               (ace-link--notmuch-hello-collect)
               #'avy--overlay-pre))))
    (ace-link--notmuch-hello-action pt)))

(define-key notmuch-hello-mode-map "o" 'ace-link-notmuch-hello)

(defun ace-link--notmuch-show-collect ()
  "Collect the positions of visible links in `notmuch-show' buffer."
  (let (candidates pt)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (while (re-search-forward "https?://" nil t)
          (setq pt (- (point) (length (match-string 0))))
          (push pt candidates))))
    (nreverse candidates)))

(defun ace-link--notmuch-show-action  (pt)
  (goto-char pt)
  (browse-url-at-point))

(defun ace-link-notmuch-show ()
  "Open a visible link in `notmuch-show' buffer."
  (interactive)
  (let ((pt (avy-with ace-link-notmuch-show
              (avy--process
               (ace-link--notmuch-show-collect)
               #'avy--overlay-pre))))
    (ace-link--notmuch-show-action pt)))

(define-key notmuch-show-mode-map "o" 'ace-link-notmuch-show)

(provide 'ace-link-notmuch)
