;;; chunyang-display-mail-mode.el --- Display how many unread mails in the mode line  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;; Display how many unread mails on the mode line.
;;
;; - It uses IMAP, you need to supply your own auth details (IMAP server, username and password)
;; - The entry is the global minor mode chunyang-display-mail-mode
;; - It refreshs 1 min after Emacs idles every time
;; - To refresh now, type M-x chunyang-display-mail-mode-count

;;; Code:

(require 'imap)

(defvar chunyang-display-mail-mode-count 0)

(defun chunyang-display-mail-mode-count ()
  (interactive)
  (setq chunyang-display-mail-mode-count
        (let* ((server "imap.fastmail.com")
               (user user-mail-address)
               (pass (when-let ((auth (car (auth-source-search
                                            :host server
                                            :user user
                                            :max 1)))
                                (pass (plist-get auth :secret)))
                       (funcall pass))))
          (with-current-buffer (imap-open server nil 'tls)
            (imap-authenticate user pass)
            (imap-mailbox-select "INBOX" 'read-only)
            (prog1 (length (imap-search "UNSEEN"))
              (imap-close))))))

(defvar chunyang-display-mail-mode-timer nil)

(define-minor-mode chunyang-display-mail-mode
  "Display how many unread mails in the mode line."
  :global t
  :lighter (:eval
            (when (> chunyang-display-mail-mode-count 0)
              (format " %d 📧" chunyang-display-mail-mode-count)))
  (cond (chunyang-display-mail-mode
         (chunyang-display-mail-mode-count)
         (setq chunyang-display-mail-mode-timer
               (run-with-idle-timer 60 t #'chunyang-display-mail-mode-count)))
        (t
         (when (timerp chunyang-display-mail-mode-timer)
           (cancel-timer chunyang-display-mail-mode-timer)
           (setq chunyang-display-mail-mode-timer nil)))))

(provide 'chunyang-display-mail-mode)
;;; chunyang-display-mail-mode.el ends here
