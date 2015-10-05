(require 'helm)

;;; TODO: Write a helm source class (just for fun)

(defun helm-joe ()
  "Preconfigured ‘helm’ for the joe shell command.

joe is a .gitignore generator, for more info, see URL
`https://github.com/karan/joe'."
  (interactive)
  (helm :sources
        (helm-build-sync-source "joe"
          :candidates
          (lambda ()
            (split-string
             (shell-command-to-string "joe ls")
             ", " :omit-nulls))
          :candidate-number-limit 999
          :persistent-action (lambda (_candidate) (ignore))
          :persistent-help "Do nothing"
          :action '(("Save to Kill Ring" .
                     (lambda (candidate)
                       (kill-new
                        (shell-command-to-string
                         (format "joe %s" (shell-quote-argument candidate))))))))
        :buffer "*helm joe*"))
