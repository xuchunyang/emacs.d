;;; chrome-cli.el --- Control Chrome via chrome-cli in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
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

;; https://github.com/prasmussen/chrome-cli

;;; Code:

(eval-when-compile
  (require 'rx)

  (declare-function helm "helm" (&rest plist))
  (declare-function helm-make-source "helm-source" (name class &rest args)))

(defvar chrome-cli-program "chrome-cli"
  "The chrome-cli program.")

;;;###autoload
(defun chrome-cli-switch-to-tab ()
  (interactive)
  (require 'helm)
  (helm
   :sources
   (helm-make-source "Chrome Tabs" 'helm-source-sync
     :candidates
     (mapcar
      (lambda (x)
        (pcase-exhaustive x
          ;; 4453 is the tab id, 4439 is the window id:
          ;; [4453] Hacker News
          ;; [4439:4453] Hacker News
          ((rx bos "[" (opt (1+ num) ":") (let id (1+ num)) "] " (let title (1+ anything)) eos)
           (cons title id))))
      (process-lines chrome-cli-program "list" "tabs"))
     :action
     (lambda (id)
       (with-temp-buffer
         (unless (zerop (call-process chrome-cli-program nil nil nil "activate" "-t" id))
           (error "activate tab failed: %s" (buffer-string))))
       ;; FIXME chrome-cli can't focus window, the following doesn't work for multiple window
       ;;       see https://github.com/prasmussen/chrome-cli/issues/35
       (call-process "open" nil nil nil "-a" "Google Chrome")))))

(provide 'chrome-cli)
;;; chrome-cli.el ends here
