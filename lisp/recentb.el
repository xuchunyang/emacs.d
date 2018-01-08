;;; recentb.el --- Reopen recently closed buffers    -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: buffers

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

(defvar recentb-list nil)

(defvar recentb-exclude (list (rx string-start " ")
                              (rx string-start "*helm")))

(defun recentb-include-p (buffer-name)
  "Return non-nil if BUFFER-NAME should be included in the recent list.
That is, if it doesn't match any of the `recentb-exclude' checks."
  (let ((checks recentb-exclude)
        (keepit t))
    (while (and checks keepit)
      (setq keepit (not (string-match (car checks) buffer-name))
            checks (cdr checks)))
    keepit))

(defun recentb-track-closed-buffer ()
  (when (recentb-include-p (buffer-name))
    (let ((-buffer-name (buffer-name))
          ;; Prefix with '-' to make lexical-binding work (am I right?)
          ;;
          ;; (special-variable-p 'major-mode)
          ;; (special-variable-p 'default-directory)
          ;; (special-variable-p 'buffer-file-name)
          ;;
          ;; all returns t
          ;;
          ;; (funcall (let ((default-directory "/")) (lambda () default-directory)))
          ;;
          ;; always returns t even if lexical-binding is t
          (-buffer-file-name buffer-file-name)
          (-major-mode major-mode)
          (-default-directory default-directory))
      (let ((elt
             (list :description -buffer-name ; TODO: More descriptive and unique
                   :open-function
                   (cond ((eq -major-mode 'Info-mode)
                          (let ((node (format "(%s) %s"
                                              (file-name-nondirectory Info-current-file)
                                              Info-current-node)))
                            (lambda () (info node))))
                         ;; TODO: Support more situations like Help, Man, Eshell
                         ;; Maybe take a look at bookmark & org-mode for inspiration
                         ((eq -major-mode 'custom-theme-choose-mode)
                          #'customize-themes)
                         ((string= -buffer-name "*Org Agenda*")
                          #'org-agenda)
                         ((string= -buffer-name "*About GNU Emacs*")
                          #'about-emacs)
                         ((string= -buffer-name "*Help*")
                          (let ((help-fn (car help-xref-stack-item))
                                (help-args (cdr help-xref-stack-item))
                                ;; (pos (point))
                                )
                            (lambda ()
                              (cond ((get-buffer "*Help*")
                                     #'view-help-buffer)
                                    (help-fn
                                     (apply help-fn help-args))
                                    (t
                                     (message "recentb: don't know how to open this help buffer"
                                              -buffer-name))))))
                         (-buffer-file-name
                          (lambda () (find-file -buffer-file-name)))
                         (t
                          (lambda ()
                            (message "recentb: don't know how to open this buffer %s"
                                     -buffer-name)))))))
        ;; FIXME Use more precise way to test for duplicates
        (setq recentb-list (remove elt recentb-list))
        (push elt recentb-list)))))

(defun recentb-open (elt)
  (funcall (plist-get elt :open-function))
  (setq recentb-list (remove elt recentb-list)))

(defun recentb-reopen-closed-buffer ()
  "Reopen last closed buffer."
  (interactive)
  (let ((elt (car recentb-list)))
    (if elt
        (recentb-open elt)
      (message "`recentb-list' is empty"))))

(defun helm-recentb ()
  (interactive)
  (require 'helm)
  (if recentb-list
      (helm :sources
            (helm-build-sync-source "Recent closed buffers"
              :candidates
              (mapcar
               (lambda (elt)
                 (cons (plist-get elt :description)
                       elt))
               recentb-list)
              :action
              (helm-make-actions
               "Open"
               #'recentb-open
               "Delete from recentb (for debugging)"
               (lambda (elt)
                 (setq recentb-list
                       (--remove-first (equal elt it) recentb-list)))))
            :buffer "*helm recentb*")
    (message "`recentb-list' is empty")))

(define-minor-mode recentb-mode
  "Toggle Recentb mode."
  :global t
  (if recentb-mode
      (add-hook 'kill-buffer-hook #'recentb-track-closed-buffer)
    (remove-hook 'kill-buffer-hook #'recentb-track-closed-buffer)))

(provide 'recentb)
;;; recentb.el ends here
