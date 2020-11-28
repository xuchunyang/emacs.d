;;; telegram-bot.el --- x                            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>

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

(require 'json)
(require 'auth-source)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar telegram-bot-token
  (funcall
   (plist-get
    (car
     (auth-source-search :host "api.telegram.org" :max 1))
    :secret)))

(defun telegram-bot-endpoint (method)
  (format "https://api.telegram.org/bot%s/%s"
          telegram-bot-token
          method))

(defun telegram-bot-url (method &optional params)
  (let ((base (telegram-bot-endpoint method)))
    (if (null params)
        base
      (format "%s?%s"
              (telegram-bot-endpoint method)
              (mapconcat
               (pcase-lambda (`(,key . ,val))
                 (format "%s=%s"
                         (url-hexify-string (symbol-name key))
                         (if (numberp val)
                             (number-to-string val)
                           (url-hexify-string val))))
               params
               "&")))))

(defun telegram-bot-json-read ()
  (message "HTTP STATUS: %s" url-http-response-status)
  (goto-char (point-min))
  (if (not (re-search-forward "application/json" url-http-end-of-headers t))
      (let ((server-error
             (message "content-type is not json, skipping, see %s" (current-buffer))))
        `((server-error . ,server-error)))
    (set-buffer-multibyte t)
    (goto-char url-http-end-of-headers)
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type  'list)
              (json-key-type    'symbol)
              (json-false       nil)
              (json-null        nil))
          (prog1 (json-read)
            (kill-buffer)))
      (json-readtable-error
       ;; seems not working, no buffer shows when error occurs
       (display-buffer (current-buffer))
       (signal (car err) (cdr err))))))

(defun telegram-bot-getUpdates (&optional offset)
  (interactive)
  (let ((url (if offset
                 (telegram-bot-url "getUpdates" `((offset . ,offset)))
               (telegram-bot-url "getUpdates"))))
    (message "Fetching %s" url)
    (url-retrieve
     url
     (lambda (_status)
       (let ((data (telegram-bot-json-read)))
         (let-alist data
           (if (not .ok)
               (message "getUpdates not ok: %s" data)
             (let ((updates .result))
               (mapc #'telegram-bot-sendMessage updates)
               (when updates
                 (setq offset (1+ (alist-get 'update_id (car (last updates))))))
               (if (sit-for 0.1)        ; 任意键退出
                   (telegram-bot-getUpdates offset)
                 (message "telegram-bot exited")))))))
     nil 'silent)))

(defun telegram-bot-sendMessage (update)
  (let-alist update
    (message "Handle update #%d" .update_id)
    (if (not .message)
        (message "Can't handle the update, it's not message type")
      (let ((url
             (telegram-bot-url "sendMessage"
                               `((chat_id . ,.message.chat.id)
                                 (text . ,(telegram-bot-handle-message .message))))))
        (message "Fetch %s" url)
        (url-retrieve
         url
         (lambda (_status)
           (let ((data (telegram-bot-json-read)))
             (let-alist data
               (unless .ok
                 (message "sendMessage not ok: %s" data)))))
         nil 'silent)))))

;; https://core.telegram.org/bots/api#message
(defun telegram-bot-handle-message (message)
  (let ((text (alist-get 'text message))
        ;; ensure case-insensitive regexp
        (case-fold-search t))
    (pcase text
      ('nil "Bot error! missing text")
      ((rx bos "/help")
       "Use `/doc FUNCTION` to lookup docstring, e.g., `/doc car`")
      ((rx bos "/doc " (let name (1+ (not space))) (* space) eos)
       (if-let ((sym (intern-soft name))
                (_ (functionp sym))
                (doc (documentation sym)))
           doc
         (format "No documentation for %S" name)))
      (_
       (upcase text)))))

(provide 'telegram-bot)
;;; telegram-bot.el ends here
