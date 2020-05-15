;;; chunyang-racket.el --- Racket Utilities          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Keywords: 

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

(defvar racket-search-manual.rkt
  (expand-file-name
   "racket-search-manual.rkt"
   (file-name-directory
    (or load-file-name buffer-file-name))))

(defvar racket-search-manual-cache-file
  (expand-file-name "racket-search-manual-cache.txt"
                    (locate-user-emacs-file "var/")))

(defun racket-search-manual-cache-file-write ()
  (with-temp-buffer
    (unless (zerop (call-process "racket" nil t nil racket-search-manual.rkt))
      (error "%s" (buffer-string)))
    (write-region nil nil racket-search-manual-cache-file)))

(defvar chunyang-racket-search-manual-actions
  (helm-make-actions
   "Open Help in Browser"
   (lambda (candidate)
     (pcase-let* ((`(,id ,from-libs)
                   (split-string
                    candidate
                    " provided from "))
                  (`(,lib . ,_)
                   (split-string
                    from-libs
                    ", ")))
       ;; racket -e '(help first #:from lazy)'
       (call-process
        "racket" nil nil nil "--eval" (format "(help %s #:from %s)" id lib))))
   "Import"
   (lambda (candidate)
     (pcase-let* ((`(,_id ,from-libs)
                   (split-string
                    candidate
                    " provided from "))
                  (`(,lib . ,_)
                   (split-string
                    from-libs
                    ", ")))
       (save-excursion
         (goto-char (point-min))
         (cond
          ((re-search-forward (rx bol "(" symbol-start "require") nil t)
           (backward-up-list)
           (forward-sexp)
           (backward-char)
           (call-interactively #'newline)
           (insert lib))
          (t
           (forward-line)
           (insert (format "(require %s)\n" lib)))))))))

(defvar chunyang-racket-search-manual-source nil)

(defun chunyang-racket-search-manual (update)
  "Search Racket Manual, with prefix arg, update the cache."
  (interactive "P")
  (when (or update (not (file-exists-p racket-search-manual-cache-file)))
    (message "[chunyang-racket-search-manual] Creating cache...")
    (racket-search-manual-cache-file-write)
    (setq chunyang-racket-search-manual-source nil))
  (unless chunyang-racket-search-manual-source
    (setq chunyang-racket-search-manual-source
          (helm-build-in-file-source
              "Search Racket Manuals" racket-search-manual-cache-file
            :action chunyang-racket-search-manual-actions)))
  (helm :sources chunyang-racket-search-manual-source
        :buffer "*helm racket search manual*"))

(defconst chunyang-racket-version
  (with-temp-buffer
    (cl-assert (zerop (call-process "racket" nil t nil "--version")))
    (goto-char (point-min))
    (and (re-search-forward (rx "v" (group (+ num) "." (+ num))))
         (match-string 1))))

(defun chunyang-racket-search-at-point ()
  (interactive)
  (when-let ((sym (symbol-at-point))
             (url (format
                   "file:///Users/xcy/Library/Racket/%s/doc/search/index.html?q=%s"
                   chunyang-racket-version
                   (url-hexify-string
                    (symbol-name sym)))))
    (message "Opening %s" url)
    ;; https://apple.stackexchange.com/questions/337134/macos-how-to-open-a-local-html-file-with-url-parameters
    (do-applescript
     (format "tell application \"Google Chrome\" to open location \"%s\"" url))))

(bind-key "C-h C-." #'chunyang-racket-search-at-point racket-mode-map)

(provide 'chunyang-racket)
;;; chunyang-racket.el ends here
