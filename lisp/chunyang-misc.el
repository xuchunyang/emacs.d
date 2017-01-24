;;; chunyang-misc.el --- Miscellaneous               -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

;; -----------------------------------------------------------------------------
;; Fibonacci number
;;

;; 1 1 2 3 5 8 13 21 34 55 89 144 ...

(defun fib (n)
  (if (memq n '(1 2))
      1
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defun fib-list (n)
  (let (last last-last this res)
    (dolist (i (number-sequence 1 n))
      (if (memq i '(1 2))
          (setq res (cons 1 res)
                last 1
                last-last 1)
        (let ((this (+ last last-last)))
          (setq res (cons this res)
                last-last last
                last this))))
    (nreverse res)))


;;; Format Emacs Lisp
(defun chunyang-one-space (beg end &optional query-p)
  "Keep only one blank space."
  (interactive "r\nP")
  (perform-replace "\\([^ \n] \\)\\( +\\)\\([^ \n]\\)"
                   (cons (lambda (_data _count)
                           (concat (match-string 1)
                                   (match-string 3)))
                         nil)
                   query-p 'regexp nil nil nil beg end))

(defun chunyang-zero-space (beg end &optional query-p)
  "Delete blank space after (."
  (interactive "r\nP")
  (perform-replace "(\\( +\\)" "(" query-p 'regexp nil nil nil beg end))

;; Also note C-u C-M-q


;;; Download and eval
(defun chunyang-download-and-eval (url)
  (let ((f (expand-file-name (file-name-nondirectory url)
                             temporary-file-directory)))
    (url-copy-file url f)
    (load-file f)))


;; http://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer
(defun chunyang-message (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column)) (insert "\n"))
        (insert (apply 'format format args))
        (insert "\n")))))

;; Oops, the following *breaks* Emacs
;; (advice-add 'message :override #'chunyang-message)


(defun chunyang-increase-number-at-point (prefix)
  "增加光标下数字以 prefix argument (默认为 1)."
  (interactive "p")
  (when (thing-at-point-looking-at "[0-9]+")
    (let* ((beg (match-beginning 0))
           (end (match-end 0))
           (number (string-to-number (buffer-substring beg end)))
           (pos (point)))
      (delete-region beg end)
      (goto-char beg)
      (insert (number-to-string (+ number prefix)))
      (goto-char pos))))


;; Make local function definition
;; FIXME: Use `flet' instead, though it is obsolete by `cl-flet'. (NO)
;; FIXME: 用 `cl-letf' 支持 recursin
(defmacro chunyang-flet (binding &rest body)
  "Make one local function binding (allow recursin)."
  (declare (indent 1))
  (let* ((name (car binding))
         (old-def (and (fboundp name) (symbol-function name)))
         (args-and-body (cdr binding)))
    `(prog2
         (defun ,name ,@args-and-body)
         ,@body
       ,(if old-def
            `(fset ',name ,old-def)
          `(fmakunbound ',name)))))

;; (defun factorial (n)
;;   (chunyang-flet (aux
;;                   (n acc)
;;                   (if (< n 1) acc (aux (1- n) (* n acc))))
;;     (aux n 1)))

;; (factorial 3)


;; Redirect shell command output to Emacs buffer

(defun redirect-shell-output-setup ()
  "Redirect shell command ouput to /tmp/emacs will appear in the buffer *Shell Command Ouput*."
  (require 'filenotify)
  (let ((buffer (get-buffer-create "*Shell Command Output*"))
        (file "/tmp/emacs"))
    ;; If file not exist, watcher won't start
    (unless (file-exists-p file) (write-region 1 1 file))
    (buffer-disable-undo buffer)
    (file-notify-add-watch file '(change)
                           (lambda (event)
                             ;; (message "Event %S" event) ; Debug
                             (when (eq 'changed (cadr event))
                               (with-current-buffer buffer
                                 (erase-buffer)
                                 (insert-file-contents file)))))))
;; (redirect-shell-output-setup)

;; Launch isearch for other window/buffer
(defun chunyang-isearch-other-window ()
  (interactive)
  (let ((input (current-word)))
    (other-window 1)
    (isearch-forward-regexp nil t)
    (and input (isearch-yank-string input))))

(define-key ctl-x-4-map "s" #'chunyang-isearch-other-window)


(defun chunyang-random-word ()
  "Pick a randome English word."
  (interactive)
  (let ((word
         (string-trim-right
          (shell-command-to-string
           ;; NOTE: jot(1) is avaiable on BSD (such as Mac OS X). An alternative is shuf(1) from GNU coreuilts.
           "LINE=$( wc -l /usr/share/dict/words | awk '{print $1}' | xargs -I {} jot -r 1 1 {} ) && tail -n $LINE /usr/share/dict/words | head -n 1"))))
    (prog1 word
      (kill-new word)
      (message "Saved to kill-ring: %s" word))))


(defun chunyang-insert-date ()
  "Insert the date using the ISO 8601 format, '%Y-%m-%d'.
BTW, 'C-u M-! date -I RET' does the same thing."
  (interactive)
  (insert (substring
           (shell-command-to-string "date +'%Y-%m-%d'")
           0 -1)))


(defun chunyang-computer-science-glossary-计算机科学词汇表-search ()
  "Search 计算机科学里，常用词汇之译法.
See URL `https://github.com/JuanitoFatas/Computer-Science-Glossary'."
  (interactive)
  (let ((data-file
         "~/src/Computer-Science-Glossary/dict.textile"))
    (assert (file-exists-p data-file))
    (helm-do-grep-1 (list data-file))))


(defun chunyang-package-archives-use-https ()
  (setq package-archives
        (mapcar (lambda (archive)
                  (cons (car archive)
                        (replace-regexp-in-string
                         "\\`http://"
                         "https://"
                         (cdr archive)))
                  archive)
                package-archives)))

;; (chunyang-package-archives-use-https)

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
