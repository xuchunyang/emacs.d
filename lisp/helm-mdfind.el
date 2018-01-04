;;; helm-mdfind.el --- Helm interface for Mac OS X's mdfind(1)  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>
;; Keywords: convenience
;; Package-Requires: ((helm "1.7.9"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Another (traditional) Emacs interface for mdfind: https://github.com/emacsmirror/mdfind

;;; Code:

(require 'helm)
(require 'helm-types)

(defun helm-mdfind-shell-command-fn ()
  (let* (process-connection-type
         non-essential
         (cmd (format "mdfind -onlyin %s -name %s" helm-mdfind-basedir (shell-quote-argument helm-pattern)))
         (proc (start-file-process-shell-command "mdfind" helm-buffer cmd)))
    (helm-log "mdfind command:\n%s" cmd)
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory))
         (if (string= event "finished\n")
             (with-helm-window
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                       (:eval (propertize
                               (format "[mdfind process finished - (%s results)]"
                                       (max (1- (count-lines
                                                 (point-min) (point-max)))
                                            0))
                               'face 'helm-locate-finish))))
               (force-mode-line-update))
           (helm-log "Error: mdfind %s"
                     (replace-regexp-in-string "\n" "" event))))))))

(defvar helm-mdfind-basedir "~")

(defvar helm-source-mdfind nil)

(defun helm-mdfind (&optional arg)
  (interactive "P")
  (unless helm-source-mdfind
    (setq helm-source-mdfind
          (helm-build-async-source "mdfind"
            :header-name (lambda (name)
                           (format "%s in %s" name helm-mdfind-basedir))
            :candidates-process #'helm-mdfind-shell-command-fn
            :action (helm-actions-from-type-file)
            :requires-pattern 3)))
  (let ((helm-mdfind-basedir (if arg
                                 (read-directory-name "Search in directory: ")
                               (or (vc-root-dir)
                                   default-directory))))
    (helm :sources 'helm-source-mdfind
          :buffer "*helm mdfind*")))

(provide 'helm-mdfind)
;;; helm-mdfind.el ends here
