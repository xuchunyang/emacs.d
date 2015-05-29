;;; helm-commandlinefu.el --- Helm interface for commandlinefu.com  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; Package-Requires: ((emacs "24.1") (json "1.3") (let-alist "1.0.3"))
;; Keywords: commandlinefu.com
;; Version: 0.1

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

;;; Change Log:
;; 0.1   - 2015/05/30 - Created File.

;;; Code:

(require 'helm)
(require 'json)
(require 'let-alist)

(defgroup helm-commandlinefu nil
  "commandlinefu.com with helm interface."
  :group 'helm)

(defvar helm-commandlinefu--json nil)

(defun helm-commandlinefu--request (url)
  "Request URL and return JSON object."
  (let (json)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (when (not (string-match "200 OK" (buffer-string)))
        (error "Problem connecting to the server"))
      (re-search-forward "^$" nil 'move)
      (setq json (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer (current-buffer)))
    (json-read-from-string json)))

(defun helm-commandlinefu--browse-url (&optional sort-by-date)
  "Create browse url, sort by votes(if SORT-BY-DATE is non-nil, sort by date)."
  (format "http://www.commandlinefu.com/commands/browse/%s/json/"
          (if sort-by-date "" "sort-by-votes")))

(defun helm-commandlinefu--browse-candicates ()
  "Build helm source candidates for `helm-commandlinefu--browse-source'."
  (mapcar (lambda (elt)
            (let-alist elt
              (cons (concat (propertize (concat "# " .summary)
                                        'face 'font-lock-comment-face)
                            "\n" .command)
                    (list :url .url
                          :votes (string-to-number .votes)
                          :command .command
                          :summary .summary
                          :id .id))))
          (append helm-commandlinefu--json nil)))

(defvar helm-commandlinefu--browse-source
  (helm-build-sync-source "commandlinefu.com: All commands sorted by votes"
    :candidates #'helm-commandlinefu--browse-candicates
    :persistent-help "Execute command without confirm"
    :persistent-action (lambda (candidate)
                         (shell-command (plist-get candidate :command)))
    :action '(("Execute command" .
               (lambda (candidate)
                 (shell-command
                  (read-shell-command "Shell Command: "
                                      (plist-get candidate :command)))))
              ("Save command to kill-ring" .
               (lambda (candidate)
                 (kill-new (plist-get candidate :command))))
              ("Browse URL" .
               (lambda (candidate)
                 (browse-url (plist-get candidate :url)))))
    :multiline t))

;;;###autoload
(defun helm-commandlinefu-browse (&optional sort-by-date)
  "Browse the Commandlinefu.com archive, sort by votes.
If SORT-BY-DATE is non-nil, sort by date."
  (interactive "P")
  (setq helm-commandlinefu--json
        (helm-commandlinefu--request (helm-commandlinefu--browse-url
                                      sort-by-date)))
  (helm :sources 'helm-commandlinefu--browse-source
        :buffer "*helm-commandlinefu*" :full-frame t))

(provide 'helm-commandlinefu)
;;; helm-commandlinefu.el ends here
