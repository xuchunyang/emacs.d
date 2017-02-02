;;; prompt-watcher.el --- Tweak MiniBuffer prompt on-the-fly  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun prompt-watcher ()
  (let ((prompt-fn
         (lambda (prompt)
           (with-current-buffer (window-buffer (active-minibuffer-window))
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert prompt)
               (add-text-properties (point-min) (point-max)
                                    minibuffer-prompt-properties))))))
    (cond ((eq this-command 'shell-command-on-region)
           (and (equal (minibuffer-prompt) "Shell command on region: ")
                current-prefix-arg
                (funcall prompt-fn "Shell command on region and replace: ")))
          ((eq this-command 'shell-command)
           (and (equal (minibuffer-prompt) "Shell command: ")
                current-prefix-arg
                (funcall prompt-fn "Shell command on region and insert: "))))))

(define-minor-mode prompt-watcher-mode
  "Watch the minibuffer prompt and customize if asking."
  :init-value nil
  :global t
  :lighter ""
  (if prompt-watcher-mode
      (add-hook 'minibuffer-setup-hook #'prompt-watcher)
    (remove-hook 'minibuffer-setup-hook #'prompt-watcher)))


(provide 'prompt-watcher)
;;; prompt-watcher.el ends here
