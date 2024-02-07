;;; qianfan.el --- A client for Baidu Qianfan LLM api  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Xu Chunyang

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

;; 想了解下 SSE 的处理方法

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'let-alist))
(require 'seq)
(require 'stream)
(require 'spinner)

(defvar url-http-end-of-headers)

(defvar qianfan-token nil)

(defun qianfan-token ()
  (unless qianfan-token
    (setq qianfan-token
          (when-let ((plist (car (auth-source-search :host "api.baidubce.com" :max 1)))
                     (user (plist-get plist :user))
                     (pass (funcall (plist-get plist :secret))))
            (with-current-buffer
                (url-retrieve-synchronously
                 (format "https://aip.baidubce.com/oauth/2.0/token?grant_type=client_credentials&client_id=%s&client_secret=%s"
                         user
                         pass))
              (set-buffer-multibyte t)
              (goto-char (1+ url-http-end-of-headers))
              (or (let-alist (json-parse-buffer :object-type 'alist)
                    .access_token)
                  (error "无法获得 qianfan-token %s" (buffer-string)))))))
  qianfan-token)

(defvar-local qianfan-last-response 0)
(defvar-local qianfan-handle-response nil)

(defun qianfan-handle-new-content (_ _ old-len)
  (when (zerop old-len)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (forward-line (* 2 foo-last-response))
        (seq-do
         (lambda (line)
           (when (and (string-prefix-p "data: {" line)
                      (string-suffix-p "}\n" line))
             (cl-incf foo-last-response)
             (when qianfan-handle-response
               (funcall qianfan-handle-response
                        (json-parse-string (substring line 6) :object-type 'alist)))))
         (stream (current-buffer) (point) 'line))))))

(defun qianfan (question)
  (interactive (list (if (use-region-p)
                         (prog1 (string-trim
                                 (buffer-substring (region-beginning) (region-end)))
                           (deactivate-mark))
                       (read-string "Ask Qianfan: "))))
  (let ((user-buffer (current-buffer))
        (proc (make-process
               :name "curl"
               :buffer (generate-new-buffer "*curl*")
               :command (list "curl" "--silent" "--json"
                              (json-serialize `((stream . t)
                                                (messages . [((role . "user")
                                                              (content . ,question))])))
                              (format "https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/%s?access_token=%s"
                                      "completions_pro"
                                      "24.f4b93675c21a394307adb62a97ef8ff9.2592000.1708174840.282335-47373095"))
               :coding 'utf-8
               :connection-type 'pty)))
    (spinner-start)
    (with-current-buffer (process-buffer proc)
      (add-hook 'after-change-functions #'qianfan-handle-new-content nil t)
      (setq qianfan-handle-response
            (lambda (json)
              (with-current-buffer user-buffer
                (let-alist json
                  (insert .result))))))
    (set-process-sentinel
     proc
     (lambda (process event)
       (when (string= event "finished\n")
         (with-current-buffer (process-buffer process)
           (remove-hook 'after-change-functions #'qianfan-handle-new-content t)
           (kill-buffer))
         (with-current-buffer user-buffer
           (spinner-stop))
         (message "【qianfan】✅"))))))

(provide 'qianfan)
;;; qianfan.el ends here
