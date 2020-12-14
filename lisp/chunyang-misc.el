;;; chunyang-misc.el --- Miscellaneous               -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'seq)
(require 'rx)
(require 'cl-lib)
(require 'json)


;; Homepage: https://www.quotes.net
;; API doc: https://www.stands4.com/api.php
;; API sample: http://www.stands4.com/services/v2/quotes.php?uid=XXX&tokenid=XXX&searchtype=RANDOM&format=json
;; API login: https://www.fastmail.com/mail/search:STANDS4/Tc40e9b03463f7182.M55fd42e24d567a90e32ee6f6?u=629140ee
(defvar chunyang-random-quote--uid
  (let ((plist (car (auth-source-search :host "stands4.com" :max 1))))
    (plist-get plist :user)))

(defvar chunyang-random-quote--tokenid
  (let ((plist (car (auth-source-search :host "stands4.com" :max 1))))
    (funcall (plist-get plist :secret))))

(defun chunyang-random-quote ()
  (interactive)
  (with-current-buffer (url-retrieve-synchronously
                        (concat
                         "http://www.stands4.com/services/v2/quotes.php?"
                         (mapconcat
                          (pcase-lambda (`(,k . ,v))
                            (concat k "=" v))
                          `(("uid" . ,chunyang-random-quote--uid)
                            ("tokenid" . ,chunyang-random-quote--tokenid)
                            ("searchtype" . "RANDOM")
                            ("format" . "json"))
                          "&"))
                        t t)
    (goto-char (point-min))
    (re-search-forward "^\r?\n")
    (let-alist (json-read)
      (if .result.quote
          (prog1 (message "%s - %s" .result.quote .result.author)
            (kill-buffer))
        (error "Failed: %s: %s" (buffer-name) (buffer-string))))))


(defun chunyang-brush (b e)
  "给选中区域 🎨，如已有则去色."
  (interactive "r")
  (let ((orig-ov (seq-find
                  (lambda (ov) (overlay-get ov 'brush))
                  (overlays-at b))))
    (if orig-ov
        (delete-overlay orig-ov)
      (let ((ov (make-overlay b e)))
        (overlay-put ov 'brush t)
        (overlay-put ov 'face
                     ;; IDEA Color picker M-x helm-color
                     (list :foreground (seq-random-elt (defined-colors)))))))
  (deactivate-mark))

;; +/- 字号
(defun chunyang-adjust-font-size (b e)
  (interactive "r")
  (let* ((ov (or (seq-find
                  (lambda (ov) (overlay-get ov 'adjust-font-size))
                  (overlays-at b))
                 (make-overlay b e)))
         (face (overlay-get ov 'face))
         (height (or (plist-get face :height) 1.0)))
    (deactivate-mark)
    (overlay-put ov 'adjust-font-size t)
    (while (pcase (read-key (format "Type ↑ ↓ to adjust font size %f: " height))
             ('up (cl-incf height 0.2))
             ('down (cl-decf height 0.2))
             ;; Quit
             (_ nil))
      ;; IDEA Try keymap
      (overlay-put ov 'face (plist-put face :height height))
      (force-window-update))))


;;;###autoload
(defun chunyang-starify-region (b e)
  "Hide the text in the region."
  (interactive "*r")
  ;; • looks nice
  (insert (make-string (length (delete-and-extract-region b e)) ?*)))


;; Detect file type

;; http://www.sparkhound.com/blog/detect-image-file-types-through-byte-arrays
(defun chunyang-detect-file-type (data)
  ;; FIXME we need to ensure data is binary (not encoded utf-8 string)
  (pcase (string-to-list (substring data 0 4))
    ('(?M ?M 0 42) 'TIFF)
    ('(?I ?I 0 42) 'TIFF)
    ('(#x89 ?P ?N ?G) 'PNG)
    ('(#xff #xd8 #xff #xe0) 'JPEG)
    ('(#xff #xd8 #xff #xe1) 'JPEG)))

;; (with-temp-buffer
;;   (set-buffer-multibyte nil)
;;   (insert-file-contents-literally "~/play.png")
;;   (chunyang-detect-file-type (buffer-string)))
;; => PNG


;; 读取剪切板中的图片

;; https://emacs-china.org/t/markdown/9296
;; NOTE 仅在 Emacs Mac Port 下测试过，其它 Emacs 应该不行
(defun chunyang-insert-image-from-clipboard ()
  "保存剪切板图片为 clipboard.png，插入 Markdown 图片链接."
  (interactive)
  (if-let* ((s
             ;; `gui-selection-value' is not reliable
             (gui--selection-value-internal 'CLIPBOARD))
            (prop (get-text-property 0 'display s))
            (data (pcase prop (`(image . ,plist) (plist-get plist :data))))
            (file "clipboard.png"))
      (progn
        (let ((coding-system-for-write 'binary))
          (write-region data nil file))
        ;; Convert TIFF into PNG
        (shell-command (format "convert %s %s" file file))
        (cond ((derived-mode-p 'markdown-mode)
               (insert (format "![](%s)" file)))
              ((derived-mode-p 'org-mode)
               (insert (format "file:%s" file)))
              (t (insert file))))
    (user-error "No image in clipboard")))


;; 恢复最近的选中区域

(defvar-local chunyang-last-region nil
  "List of mark and point.")

(defun chunyang-last-region-save ()
  (when (/= (mark) (point))
    (setq chunyang-last-region (list (mark) (point)))))

(defun chunyang-last-region-restore ()
  "Restore the last region."
  (interactive)
  (when chunyang-last-region
    (pcase-let ((`(,mark ,point) chunyang-last-region))
      (setf (mark) mark
            (point) point))))

;; (add-hook 'deactivate-mark-hook #'chunyang-last-region-save)
;; (remove-hook 'deactivate-mark-hook #'chunyang-last-region-save)



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
    (message "%s" (substring s 2 -2))))

(defun chunyang-encode-thunder-link (link)
  "Encode LINK into thunder:// link."
  (interactive "s链接: ")
  (message "%s" (concat "thunder://" (base64-encode-string (concat "AA" link "ZZ")))))

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
          (setq version (or (magit-git-string "describe" "--tags" "--dirty")
                            (magit-rev-parse "--short" "HEAD")))
          (message "%s %s" (upcase-initials package) version)
          version)))))


;; https://github.com/melpa/melpa/pull/5008    melpa/melpa: Pull Request #5008
;; https://github.com/melpa/melpa/issues/5368  melpa/melpa: Issue #5368
;;
;; - Discourse Onebox
;; - The Open Graph protocol <http://ogp.me/>
(defun chunyang-url-humanize (url)
  (let ((user (rx (group (1+ (in alnum)))))
        (repo (rx (group (1+ (in alnum)))))
        (id   (rx (group (1+ (in digit))))))
    (cond
     ((string-match (format "https://github.com/%s/%s/pull/%s" user repo id) url)
      (format "%s/%s: Pull Request #%s" (match-string 1 url) (match-string 2 url) (match-string 3 url)))
     ((string-match (format "https://github.com/%s/%s/issues/%s" user repo id) url)
      (format "%s/%s: Issue #%s" (match-string 1 url) (match-string 2 url) (match-string 3 url)))
     ;; TODO: Parse <title> & Open Graph protocol ?
     (t (user-error "Unsupported URL")))))

;; https://emacs-china.org/t/topic/5334
(require 'async)
(require 'pp)

(defun chunyang-eval-in-other-emacs (emacs form)
  (interactive
   (list (read-shell-command "Emacs: ")
         (read--expression "Eval: ")))
  (let ((fullpath (executable-find emacs)))
    (unless fullpath
      (user-error "Command not found: %s" emacs))
    (let* ((invocation-directory (file-name-directory fullpath))
           (invocation-name (file-name-nondirectory fullpath))
           (result (async-get (async-start (lambda () (eval form))))))
      (when (called-interactively-p 'any)
        (pp-display-expression result "*Pp Eval Output*"))
      result)))


;; https://emacs-china.org/t/topic/4629/8
(defmacro chunyang-string-interpolation (string)
  (let (format-string forms)
    (setq format-string
          (replace-regexp-in-string
           "#{\\(.+\\)}"
           (lambda (substring)
             (push (read (match-string 1 substring)) forms)
             "%s")
           string))
    `(format ,format-string ,@forms)))


(defun chunyang-helm-time ()
  (interactive)
  (require 'helm)
  (helm
   :sources
   (helm-build-sync-source "Current datetime in various formats"
     :candidates
     (lambda ()
       (mapcar (pcase-lambda (`(,fmt ,val))
                 (cons (format "%-20s %s" fmt val)
                       val))
               `(
                 ;; 2019-10-28T18:07:16Z
                 ("RFC3339" ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                 ;; Mon, 28 Oct 2019 18:07:16 +0000
                 ("RFC1123" ,(format-time-string "%a, %d %b %Y %H:%M:%S %z"))
                 ;; Mon Oct 28 18:23:08 GMT 2019
                 ("date(1)" ,(format-time-string "%a %b %d %H:%M:%S %Z %Y"))
                 ;; Mon Oct 28 18:07:16 2019
                 ("ANSIC" ,(format-time-string "%a %b %d %H:%M:%S %Y"))
                 ("DATE" ,(format-time-string "%Y-%m-%d"))
                 ("TIME" ,(format-time-string "%H:%M:%S"))))))
   :buffer "*helm time*"))



(defun chunyang-indent-align-cycle ()
  "手动对齐参数位置."
  (interactive)
  (let ((beg (condition-case nil
                 (scan-lists (point) -1 1)
               (scan-error nil))))
    (when beg
      (let (columns cur-col target-col offset)
        (save-excursion
          (forward-line -1)
          (goto-char (line-end-position))
          (let ((lb (line-beginning-position)))
            (while (let ((pt (ignore-errors (scan-sexps (point) -1))))
                     (and pt (> pt lb) (goto-char pt)))
              (push (- (point) lb) columns))))
        (when columns
          (back-to-indentation)
          (setq cur-col (- (point) (line-beginning-position)))
          (or (catch 'done
                (dolist (c columns)
                  (when (> c cur-col)
                    (setq target-col c)
                    (throw 'done t))))
              (setq target-col (car columns)))
          (setq offset (- target-col cur-col))
          (cond
           ((> offset 0) (insert (make-string offset ?\s)))
           ((< offset 0) (delete-char offset))))))))


;; https://emacs.stackexchange.com/questions/33976/how-do-you-reload-a-dynamic-module
(defun chunyang-fake-module-reload (module)
  (interactive "fReload Module file: ")
  (let ((tmpfile (make-temp-file
                  (file-name-nondirectory module) nil module-file-suffix)))
    (copy-file module tmpfile t)
    (module-load tmpfile)))


;; 生成 https://glitch.com/ 风格的随机项目名称
(defun chunyang-glitch-name ()
  "Get a Glitch-style project name.

Some examples:
 curly-boggy-aries
 busy-brief-bucket
 bolder-glistening-vanadium
 melted-lemon-jet

predicates predicates objects
predicates objects objects"
  (interactive)
  (let ((file (expand-file-name "~/.emacs.d/misc/glitch-firendly-words.json")))
    (unless (file-exists-p file)
      (url-copy-file
       "https://raw.githubusercontent.com/glitchdotcom/friendly-words/master/generated/words.json"
       file))
    (let-alist (with-temp-buffer
                 (insert-file-contents-literally file)
                 (goto-char (point-min))
                 (let ((json-array-type 'list))
                   (json-read)))
      (let ((name (concat
                   (seq-random-elt .predicates) "-"
                   (seq-random-elt .predicates) "-"
                   (seq-random-elt .objects))))
        (kill-new name)
        (message "Saved to kill-ring: %s" name)
        name))))

(provide 'chunyang-misc)
;;; chunyang-misc.el ends here
