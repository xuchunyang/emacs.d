;;; chunyang-go.el --- Utilities for Go  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(require 'helm)
(require 'go-mode)

(defvar chunyang-helm-go-import-source nil)

(defun chunyang-helm-go-packages (arg)
  "With prefix ARG, reinitialize the cache."
  (interactive "P")
  (when (or arg (not chunyang-helm-go-import-source))
    (message "Building Go Packages cache (it can take a while)...")
    (setq chunyang-helm-go-import-source
          (helm-build-in-buffer-source "Go Packages"
            :data (go-packages)
            :candidate-number-limit 9999
            :action (helm-make-actions
                     "Import"
                     (lambda (package)
                       (go-import-add nil package))))))
  (helm :sources (list chunyang-helm-go-import-source)
        :preselect (and (eq (char-before) ?.)
                        (save-excursion
                          (forward-char -1)
                          (thing-at-point 'symbol)))
        :buffer "*helm Go Packages*"))

;; Port vim-go's go#doc#OpenBrowser
;; https://github.com/fatih/vim-go/blob/e3a760fd9f7eaceec49cfe4adc4d3b6602b2ff0a/autoload/go/doc.vim#L15
(defun chunyang-go-doc-browse-url ()
  (interactive)
  ;; $ gogetdoc -json -pos ~/go/src/golang.org/x/tools/cmd/godoc/main.go:#1371 | jq
  (let* ((command `("gogetdoc"
                    "-json"
                    "-pos"
                    ,(format "%s:#%d" buffer-file-name (1- (position-bytes (point))))))
         (output-buffer (generate-new-buffer " *temp*"))
         (status (apply #'call-process-region
                        nil nil (car command) nil output-buffer nil
                        (cdr command)))
         (output (with-current-buffer output-buffer (buffer-string))))
    (unwind-protect
        (if (not (zerop status))
            (error "'%s' failed: %s" (string-join command " ") output)
          (let-alist (json-read-from-string output)
            (browse-url
             (chunyang-go-doc-url .decl .import .name))))
      (kill-buffer output-buffer))))

;; Other options are:
;; - https://golang.org/pkg
;; - https://godoc.org
;;
;; M-x prodigy to start the godoc server
(defvar chunyang-go-doc-base-url "http://localhost:3000")

(defun chunyang-go-doc-url (.decl .import .name)
  (if (string-prefix-p "package" .decl)
      (format "%s/%s" chunyang-go-doc-base-url .import)
    (format "%s/%s#%s" chunyang-go-doc-base-url .import .name)))

(defun chunyang-go-code-fontify (code)
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (go-mode))
    (font-lock-ensure)
    (buffer-string)))

;; /usr/local/Cellar/go/1.13.3/libexec/src/fmt/print.go:273:6 -> fmt/print.go
;; /Users/xcy/go/src/github.com/PuerkitoBio/goquery/type.go:19:6  -> github.com/PuerkitoBio/goquery/type.go
(defun chunyang-go--shorten-filepath (filepath)
  (replace-regexp-in-string
   (rx ":" (+? nonl) eos)
   ""
   (replace-regexp-in-string
    (rx bos (+? nonl) "/src/")
    ""
    filepath)))

;; http://localhost:3000/fmt#Println -> localhost:3000/fmt#Println
(defun chunyang-go--shorten-url (url)
  (replace-regexp-in-string (rx bos "http" (opt "s") "://") "" url))

(defun chunyang-go--src (.import .name)
  (with-temp-buffer
    ;; go doc -src path/filepath.Base
    (if (zerop (call-process "go" nil t nil "doc" "-src" (format "%s.%s" .import .name)))
        (progn
          (chunyang-go-code-fontify
           (buffer-substring-no-properties (point-min) (point-max))))
      (error "go doc failed: %s" (buffer-string)))))

(defun chunyang-godoc-gogetdoc (point)
  "Like `godoc-gogetdoc' but also print source code location."
  (when (buffer-modified-p)
    (save-buffer))
  (let ((buffer (generate-new-buffer " *chunyang-godoc-gogetdoc*")))
    (unwind-protect
        (let ((app "gogetdoc")
              (args `("-json"
                      "-pos"
                      ,(format "%s:#%d" buffer-file-name (1- (position-bytes point))))))
          (unless (zerop (apply #'call-process-region nil nil app nil buffer nil args))
            (error (format "'%s' failed:\n%s"
                           (string-join (cons app args) " ")
                           (with-current-buffer buffer (buffer-string)))))
          (with-current-buffer (godoc--get-buffer "<at point>")
            (let-alist (with-current-buffer buffer
                         (goto-char (point-min))
                         (json-read))

              ;; Source code
              (unless (string-prefix-p "package" .decl)
                (insert (chunyang-go--src .import .name) ?\n))
              
              ;; link to local source file
              (insert-text-button
               (chunyang-go--shorten-filepath .pos)
               'action
               (lambda (_)
                 (pcase-exhaustive (split-string .pos ":")
                   (`(,filename ,line ,column)
                    (set-buffer (find-file-noselect filename))
                    (server-goto-line-column
                     (cons (string-to-number line)
                           (string-to-number column)))
                    (switch-to-buffer (current-buffer))))))

              (insert " | ")

              ;; link to godoc website
              (let ((url (chunyang-go-doc-url .decl .import .name)))
                (insert-button (chunyang-go--shorten-url url)
                               'face 'link
                               'action (lambda (_button) (browse-url url))
                               'help-echo "mouse-2, RET: Follow this link"
                               'follow-link t))
              (insert ?\n))
            (goto-char (point-min))
            (godoc-mode)
            ;; FIXME: Reuse existing window
            (display-buffer (current-buffer) t)))
      (kill-buffer buffer))))

(provide 'chunyang-go)
;;; chunyang-go.el ends here
