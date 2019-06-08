;;; helm-chrome-history.el --- Search Google Chrome History with Helm  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: convenience, tools

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

;; Search Chrome History with Helm

;;; Code:

(require 'helm)

(defvar helm-chrome-history-db
  "~/Library/Application Support/Google/Chrome/Profile 1/History"
  "Chrome History SQLite Database.")

;; FIXME: How to reset the cache?
(defun helm-chrome-history-init ()
  (unless (helm-candidate-buffer)
    (helm-init-candidates-in-buffer 'global
      (with-temp-buffer
        (let ((db "/tmp/History"))
          (copy-file helm-chrome-history-db db t)
          (unless (zerop (call-process "sqlite3" nil t nil db "select url, title from urls"))
            (error "sqlite3 failed: %s" (buffer-string)))
          (buffer-string))))))

(defvar helm-chrome-history-action
  (helm-make-actions
   "Browse URL"
   (lambda (x) (browse-url (car (split-string x "|"))))
   "EWW URL"
   (lambda (x) (eww (car (split-string x "|"))))))

(defvar helm-chrome-history-source
  (helm-build-in-buffer-source "Chrome History"
    :init #'helm-chrome-history-init
    :action helm-chrome-history-action))

;;;###autoload
(defun helm-chrome-history ()
  "Brwose Chrome History with helm."
  (interactive)
  (helm :sources helm-chrome-history-source
        :buffer "*Helm Chrome History*"
        :full-frame t))

(provide 'helm-chrome-history)
;;; helm-chrome-history.el ends here
