;;; chunyang-misc.el --- Miscellaneous               -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'rx)
(require 'cl-lib)


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
(require 'thingatpt)

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
(require 'filenotify)

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
  "Pick a randome English word from /usr/share/dict/words."
  (interactive)
  (with-temp-buffer
    (if (zerop (call-process-shell-command
                "shuf -n 1 /usr/share/dict/words" nil t))
        (let ((word (buffer-substring (point-min) (1- (point-max)))))
          (prog1 word
            (kill-new word)
            (message "Saved to kill-ring: %s" word)))
      (error "%s" (buffer-string)))))

(defun chunyang-goto-random-line ()
  (interactive)
  (goto-char (point-min))
  (forward-line (random (line-number-at-pos (point-max)))))


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
    (cl-assert (file-exists-p data-file))
    (declare-function helm-do-grep-1 "helm-grep")
    (helm-do-grep-1 (list data-file))))


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

(defun chunyang-format-as-binary (n)
  "Convert Number N into String in binary format."
  (let (res)
    (if (= n 0)
        (setq res '(0))
      (while (> n 0)
        (push (% n 2) res)
        (setq n (/ n 2))))
    (concat "#b" (mapconcat #'number-to-string res ""))))

;; (chunyang-format-as-binary #b100)
;;      => "#b100"


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


;;; Debug Emacs init file

(defun chunyang-open-another-emacs ()
  "Open another Emacs instance without closing the current one.
For testing / debugging Emacs init file."
  (interactive)
  ;; XXX: Make it work under other environment ?
  (defvar *is-mac*)
  (unless (and *is-mac* (display-graphic-p))
    (user-error "Unsupported platform or situation."))
  (let ((app (replace-regexp-in-string
              "/Contents/MacOS/"
              ""
              invocation-directory)))
    (call-process-shell-command
     (read-shell-command
      "Shell command: "
      ;; Initial input
      (format "open -n -a %s" (shell-quote-argument app))
      nil
      ;; Default value, just in case I forget how to pass arguments
      ;; through open, i.e., --args
      (format "open -n -a %s --args -Q" (shell-quote-argument app))))))


;;; Timer | Stopwatch

(defun chunyang-timer ()
  "Stopwatch."
  (interactive)
  (let ((t1 (current-time))
        t2)
    (while (/= ?\C-g
               (read-key
                (concat (current-time-string t1) " | "
                        "Timer started. Stop with C-g"))))
    (setq t2 (current-time))
    (message "%s - %s = %fs"
             (current-time-string t1)
             (current-time-string t2)
             (float-time (time-subtract t2 t1)))))


;;; QR Code

(defun chunyang-scan-qr-code-from-screen ()
  (interactive)
  (let ((res (shell-command-to-string
              "scan-qr-code-from-screen")))
    (unless (string= res "")
      ;; Remove the final newline
      (message "%s" (substring res 0 -1)))))

(defun chunyang-qrdecode (png)
  "Decode QR Code in PNG image."
  (let ((res (shell-command-to-string
              (concat "qrdecode " png))))
    (unless (string= res "")
      (substring res 0 -1))))

(defun chunyang-qrencode (string png)
  "Encode STRING in a QR Code and save as PNG."
  (with-temp-buffer
    (unless (zerop (call-process "qrencode" nil t nil string "-o"
                                 (expand-file-name png)))
      (error "%s" (buffer-string)))))


;;; 迅雷链接 thunder://

(defun chunyang-decode-thunder-link (link)
  "Decode thunder:// LINK."
  (interactive "s迅雷链接: ")
  (unless (string-prefix-p "thunder://" link)
    (error "%s is not a thunder:// link" link))
  (let ((s (base64-decode-string (substring link (length "thunder://")))))
    (unless (and (string-prefix-p "AA" s)
                 (string-suffix-p "ZZ" s))
      (error "%s is not started with 'AA' or ended with 'ZZ'" link))
    (substring s 2 -2)))

(defun chunyang-encode-thunder-link (link)
  "Encode LINK into thunder:// link."
  (interactive "s链接: ")
  (concat "thunder://" (base64-encode-string (concat "AA" link "ZZ"))))

;; (chunyang-encode-thunder-link "http://example.com/index.html")
;;      => "thunder://QUFodHRwOi8vZXhhbXBsZS5jb20vaW5kZXguaHRtbFpa"

;; (chunyang-decode-thunder-link
;;  "thunder://QUFodHRwOi8vZXhhbXBsZS5jb20vaW5kZXguaHRtbFpa")
;;      => "http://example.com/index.html"


;;; Markup Links

(defun chunyang-markdown-link-parse (link)
  "A simple parser of Markdown link.

(chunyang-markdown-link-parse \"[Example Domain](https://example.com)\")
     => (\"Example Domain\" \"https://example.com\")
"
  (if (string-match (rx "[" (group (0+ anything)) "]"
                        "(" (group (0+ anything)) ")")
                    link)
      (list (match-string 1 link)
            (match-string 2 link))
    (error "Cannot parse %s as Markdown link" link)))

(defun chunyang-markdown-link-make (text link)
  (format "[%s](%s)" text link))

(defun chunyang-org-link-parse (link)
  "A simple parser of Org link.

(chunyang-org-link-parse \"[[https://emacs-china.org/][Emacs China]]\")
     => (\"https://emacs-china.org/\" \"Emacs China\")
"
  (if (string-match
       (rx "[[" (group (0+ anything)) "][" (group (0+ anything)) "]]")
       link)
      (list (match-string 1 link)
            (match-string 2 link))
    (error "Cannot parse %s as Org link" link)))

(defun chunyang-org-link-make (text link)
  (format "[[%s][%s]]" link text))

(defun chunyang-markdown-link->org-link (markdown)
  (apply #'chunyang-org-link-make (chunyang-markdown-link-parse markdown)))

(defun chunyang-org-link->markdown-link (org)
  (apply #'chunyang-markdown-link-make (chunyang-org-link-parse org)))


(defun chunyang-straight-emacs-Q-command (package)
  (interactive
   (list
    (straight--select-package "Package" nil 'installed)))
  (let ((cmd (mapconcat
              #'shell-quote-argument
              `(,(concat invocation-directory invocation-name)
                "-Q" "--eval" "(setq debug-on-error t)"
                "--load" "~/.emacs.d/straight/repos/straight.el/bootstrap.el"
                "--eval" ,(format "(straight-use-package '%s)" package))
              " ")))
    (message
     "Uncustomized %s command saved to kill-ring, please run it in a terminal"
     package)
    (kill-new cmd)))

;; Inspired by `magit-version'
(defun chunyang-straight-git-version (package)
  (interactive
   (list
    (straight--select-package "Package" nil 'installed)))
  (let ((recipe (gethash package straight--recipe-cache))
        version)
    (straight--with-plist recipe
        (local-repo type)
      (when (and (eq type 'git) local-repo)
        (let ((default-directory (straight--repos-dir local-repo)))
          (setq version (magit-git-string "describe" "--tags" "--dirty"))
          (message "%s" version)
          version)))))

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
