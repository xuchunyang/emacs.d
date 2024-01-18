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

(defvar-local qianfan-last-response 0)

(defun qianfan-handle-new-content (_ _ old-len)
  (when (= old-len 0)
    (save-excursion
      (save-match-data
        ;; Skip the headers
        (if (bound-and-true-p url-http-end-of-headers)
            (goto-char (1+ url-http-end-of-headers))
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t))
        
        (when-let ((end (re-search-forward "\n\n" nil t qianfan-last-response))
                   (start
                    (progn (forward-line -2)
                           (+ (point) (length "data: "))))
                   (json (json-parse-string
                          (buffer-substring-no-properties start end)
                          :object-type 'alist)))
          (cl-incf qianfan-last-response)
          (message "%S" json)
          (with-current-buffer "*scratch*"
            (let-alist json
              (insert .result))))))))

(let
    ((url-request-method "POST")
     (url-request-extra-headers
      '(("Content-Type" . "application/json")))
     ;; https://github.com/ahyatt/llm/blob/97933359cb4f1bf4b03ded5ae43ea3360b818e77/llm-request.el#L129C9-L129C46
     (url-mime-encoding-string "identity")
     (url-request-data (encode-coding-string
                        "{\"stream\": true, \"messages\":[{\"role\":\"user\",\"content\":\"请以保护环境为主题，写一篇作文\"}]}"
                        'utf-8)))
  (with-current-buffer
      (url-retrieve
       "https://aip.baidubce.com/rpc/2.0/ai_custom/v1/wenxinworkshop/chat/eb-instant?access_token=24.f4b93675c21a394307adb62a97ef8ff9.2592000.1708174840.282335-47373095"
       (lambda (_)
         (remove-hook 'after-change-functions #'qianfan-handle-new-content t)))
    (set-buffer-multibyte t)
    (add-hook 'after-change-functions #'qianfan-handle-new-content nil t)
    (pop-to-buffer-same-window (current-buffer))))

(provide 'qianfan)
;;; qianfan.el ends here
