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


;; -----------------------------------------------------------------------------
;; Run-length
;;

;; https://en.wikipedia.org/wiki/Run-length_encoding

(defun run-length-encode (str)
  "Return Run-Length representation of STR as a string."
  (with-temp-buffer
    (let ((idx 0)
          (len (length str))
          this-char last-char occur)
      (while (< idx len)
        (let ((this-char (aref str idx)))
          (if (eq this-char last-char)
              (setq occur (+ 1 occur))
            (if last-char
                (insert (format "%d%c" occur last-char)))
            (setq last-char this-char
                  occur 1))
          (if (= (+ 1 idx) len)
              (insert (format "%d%c" occur last-char))))
        (setq idx (+ 1 idx))))
    (buffer-string)))

(defun run-length-decode (str)
  "Decode Run-Length representation."
  (with-temp-buffer
    (let ((idx 0)
          this-char occur char)
      (while (< idx (length str))
        (let ((this-char (aref str idx)))
          (if (< ?0 this-char ?9)
              (setq occur (concat occur (char-to-string this-char)))
            (setq char this-char)
            (insert (make-string (string-to-number occur) char))
            (setq occur nil)))
        (setq idx (+ 1 idx))))
    (buffer-string)))

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
