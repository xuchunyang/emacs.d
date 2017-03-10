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


;;; Download and Eval file written in Emacs Lisp

(defun chunyang-download-and-load (url)
  "Download an Emacs Lisp file from URL and load it."
  (interactive "sURL of the Emacs Lisp file to load: ")
  (let ((f (expand-file-name (file-name-nondirectory url)
                             temporary-file-directory)))
    (url-copy-file url f)
    (load-file f)))


;;; Keep text properties in `message'

;; See http://emacs.stackexchange.com/questions/20171/how-to-preserve-color-in-messages-buffer

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


;;; Increase and Decrease number at point

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


;;; Make local function definition

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


;;; Redirect shell command output to Emacs buffer

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

;;; Launch isearch for other window/buffer

(defun chunyang-isearch-other-window ()
  (interactive)
  (let ((input (current-word)))
    (other-window 1)
    (isearch-forward-regexp nil t)
    (and input (isearch-yank-string input))))

(define-key ctl-x-4-map "s" #'chunyang-isearch-other-window)


;;; Random

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

(defun chunyang-goto-random-line ()
  (interactive)
  (goto-line (1+ (random (line-number-at-pos (point-max))))))


;;; Timestamps

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


;;; Change http to https in `package-archives'

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


;;; 转置矩阵 - Transpose

(defun chunyang-transpose (lists)
  "Return the transpose of a martrix LISTS.
See URL `https://en.wikipedia.org/wiki/Transpose'."
  ;; NOTE assuming LISTS is a valid martrix
  (cl-loop for idx from 0 to (1- (length (car lists)))
           collect (cl-loop for lst in lists
                            collect (nth idx lst))))

;; (chunyang-transpose '((1 3 5)
;;                       (2 4 5)))
;;      => ((1 2) (3 4) (5 5))


;;; Format binary number

(defun chunyang-format-as-binary (number)
  ;; FIXME: It might be better to calculate directly
  (let ((map '((?0 . "000")
               (?1 . "001")
               (?2 . "010")
               (?3 . "011")
               (?4 . "100")
               (?5 . "101")
               (?6 . "110")
               (?7 . "111"))))
    (replace-regexp-in-string
     "\\`0+" ""
     (mapconcat #'identity
                (mapcar (lambda (c) (cdr (assq c map)))
                        (string-to-list (format "%o" number))) ""))))


;;; Public IP & Location

;; XXX: Oops, '$ curl ip.cn' just works, no needs to parse HTML
(defun chunyang-get-ip-location (&optional ip)
  "Get location of IP from http://ip.cn.
With prefix argument, IP is prompted."
  (interactive
   (list (and current-prefix-arg (read-string "IP: "))))
  (with-current-buffer (url-retrieve-synchronously
                        (if ip
                            (concat "http://ip.cn/index.php?ip=" ip)
                          "http://ip.cn/"))
    (set-buffer-multibyte t)
    ;; Delete header
    (ignore-errors
      (goto-char (point-min))
      (delete-region (point-min)
                     (1+ (re-search-forward "^$" nil t))))
    (let* ((divs
            (let-alist (libxml-parse-html-region (point-min) (point-max)) .body.div))
           (div
            (cl-find-if
             (lambda (elt)
               (and (listp elt)
                    (equal (cdr (assq 'id (plist-get elt 'div)))
                           "result")))
             divs)))
      (seq-let (_class p1 p2 p3) (cdr (assq 'div (cdr div)))
        (let ((ip (car (last (car (last p1)))))
              (location (car (last (car (last p2)))))
              (geoip (car (last p3))))
          (message "IP: %s | 地理位置: %s | GeoIP: %s"
                   ip location geoip)
          (list ip location geoip))))))

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
