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

;; NOTE 发现有重复的结果
(defun qianfan-handle-new-content (_ _ old-len)
  (when (= old-len 0)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t (1+ qianfan-last-response))

        (while (re-search-forward "\n\n" nil t)

          (when-let* ((str (save-excursion
                             (forward-line -2)

                             (buffer-substring-no-properties
                              (point)
                              (line-end-position))))
                      (data (progn
                              ;; (message "%S" str)
                              (when (string-prefix-p "data: " str)
                                (ignore-errors
                                  (json-parse-string
                                   (substring str (length "data: "))
                                   :object-type 'alist))))))
            (message "%s" data)
            (cl-incf qianfan-last-response)
            ;; (message "%S" data)
            ;; (when qianfan-handle-response
            ;;   (funcall qianfan-handle-response data))
            ))))))

(defun qianfan (question)
  (interactive "s文心一言: ")
  (let ((user-buffer (current-buffer)))
    (let ((url-request-method "POST")
          (url-request-extra-headers '(("Content-Type" . "application/json")))
          ;; (url-mime-encoding-string "identity")
          (url-request-data (encode-coding-string
                             (json-serialize
                              `((stream . t)
                                (messages . [((role . "user")
                                              (content . ,question))])))
                             'utf-8)))
      (with-current-buffer
          (url-retrieve
           (concat "https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/eb-instant?access_token="
                   (qianfan-token))
           (lambda (_)
             (remove-hook 'after-change-functions #'qianfan-handle-new-content t)))
        ;; (set-buffer-multibyte t)
        (add-hook 'after-change-functions #'qianfan-handle-new-content nil t)
        (setq qianfan-handle-response
              (lambda (json)
                (with-current-buffer user-buffer
                  (let-alist json
                    (insert .result)))))
        (pop-to-buffer-same-window (current-buffer))))))

(provide 'qianfan)
;;; qianfan.el ends here
