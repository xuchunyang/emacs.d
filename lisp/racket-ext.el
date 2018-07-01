;; Provide `racket-shell-send-string-no-output' like
;; `python-shell-send-string-no-output'

(require 'racket-mode)

(defun racket-shell-comint-end-of-output-p (output)
  "Return non-nil if OUTPUT ends with input prompt."
  (string-match
   (concat
    "\r?\n?"
    ;; Remove initial caret from prompt regexp
    (replace-regexp-in-string
     (rx string-start ?^) ""
     comint-prompt-regexp)
    (rx eos))
   output))

(defvar racket-shell-output-filter-in-progress nil)
(defvar racket-shell-output-filter-buffer nil)

(defun racket-shell-output-filter (string)
  "Filter used in `racket-shell-send-string-no-output' to grab output.
STRING is the output received to this point from the process.
This filter saves received output from the process in
`racket-shell-output-filter-buffer' and stops receiving it after
detecting a prompt at the end of the buffer."
  (setq racket-shell-output-filter-buffer
        (concat racket-shell-output-filter-buffer string))
  (when (racket-shell-comint-end-of-output-p
         racket-shell-output-filter-buffer)
    ;; Output ends when `racket-shell-output-filter-buffer' contains
    ;; the prompt attached at the end of it.
    (setq racket-shell-output-filter-in-progress nil
          racket-shell-output-filter-buffer
          (substring racket-shell-output-filter-buffer
                     0 (match-beginning 0))))
  "")

(defun racket-shell-send-string-no-output (string)
  "Send STRING to PROCESS and inhibit output.
Return the output."
  (let ((process (or (racket--get-repl-buffer-process)
                     (error "No racket process")))
        (comint-preoutput-filter-functions
         '(racket-shell-output-filter))
        (racket-shell-output-filter-in-progress t)
        (inhibit-quit t))
    (or
     (with-local-quit
       (comint-send-string process string)
       (while racket-shell-output-filter-in-progress
         ;; `racket-shell-output-filter' takes care of setting
         ;; `racket-shell-output-filter-in-progress' to NIL after it
         ;; detects end of output.
         (accept-process-output process))
       (prog1
           racket-shell-output-filter-buffer
         (setq racket-shell-output-filter-buffer nil)))
     (with-current-buffer (process-buffer process)
       (comint-interrupt-subjob)))))

;; (racket-shell-send-string-no-output "(version)\n")
;; (racket-shell-send-string-no-output "(for/list ([i (in-range 1 10 2)]) i)\n")
;; => "'(1 3 5 7 9)"

(provide 'racket-ext)
