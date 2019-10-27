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
             (chunyang-godoc-browser-url .decl .import .name))))
      (kill-buffer output-buffer))))

(defun chunyang-go-doc-url (.decl .import .name)
  (if (string-prefix-p "package" .decl)
      (format "https://godoc.org/%s" .import)
    (format "https://godoc.org/%s#%s" .import .name)))

(defun chunyang-go-code-fontify (code)
  (with-temp-buffer
    (insert code)
    (delay-mode-hooks (go-mode))
    (font-lock-ensure)
    (buffer-string)))

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
              ;; Source
              (insert-text-button
               .pos
               'action
               (lambda (_)
                 (pcase-exhaustive (split-string .pos ":")
                   (`(,filename ,line ,column)
                    (set-buffer (find-file-noselect filename))
                    (server-goto-line-column
                     (cons (string-to-number line)
                           (string-to-number column)))
                    (switch-to-buffer (current-buffer))))))
              (insert ?\n)

              ;; Documentation
              (insert (chunyang-go-code-fontify
                       (format "package %s // import \"%s\"\n" .pkg .import))
                      ?\n)
              (insert (chunyang-go-code-fontify .decl) ?\n ?\n)
              (insert .doc ?\n)

              ;; Godoc reference
              (let ((url (chunyang-godoc-browser-url .decl .import .name)))
                (insert-button url
                               'face 'link
                               'action (lambda (_button) (browse-url url))
                               'help-echo "mouse-2, RET: Follow this link"
                               'follow-link t)
                (insert ?\n)))
            (goto-char (point-min))
            (godoc-mode)
            ;; FIXME: Reuse existing window
            (display-buffer (current-buffer) t)))
      (kill-buffer buffer))))

(provide 'chunyang-go)
;;; chunyang-go.el ends here
