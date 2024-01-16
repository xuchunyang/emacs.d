;;; wolai.el --- Add notes to Wolai (我来)                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>
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

;; 使用我来 API 添加笔记.

;;; Code:

(defvar wolai-token nil)

(defun wolai--request (method url body &optional extra-headers)
  (let ((url-request-method method)
        (url-request-extra-headers (cons '("Content-Type" . "application/json")
                                         extra-headers))
        (url-request-data (encode-coding-string (json-encode body) 'utf-8)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun wolai-token ()
  (unless wolai-token
    (setq wolai-token
          (let-alist (wolai--request
                      "POST"
                      "https://openapi.wolai.com/v1/token"
                      (let ((plist (car (auth-source-search :host "openapi.wolai.com" :max 1))))
                        (unless plist
                          (user-error "Can't find host: %s" host))
                        (let ((appId (plist-get plist :user))
                              (appSecret (funcall (plist-get plist :secret))))
                          `((appId . ,appId)
                            (appSecret . ,appSecret)))))
            .data.app_token)))
  wolai-token)

;; https://www.wolai.com/rp8TYN2cV3QiKN8YpjPb9D

(wolai--request
 "POST"
 "https://openapi.wolai.com/v1/blocks"
 '((parent_id . "uukdKTgw6VLLuyvgrvE8k3")
   (blocks . [((type . "text")
               (content . "hello"))]))
 `(("Authorization" . ,(wolai-token))))

(wolai--request
 "POST"
 "https://openapi.wolai.com/v1/blocks"
 '((parent_id . "uukdKTgw6VLLuyvgrvE8k3")
   (blocks . [((type . "heading")
               (level . 1)
               (content . "hello heading 1"))]))
 `(("Authorization" . ,(wolai-token))))

(wolai--request
 "POST"
 "https://openapi.wolai.com/v1/blocks"
 '((parent_id . "uukdKTgw6VLLuyvgrvE8k3")
   (blocks . [((type . "code")
               (language . "lisp")
               (content . "(list 1 2 3)"))]))
 `(("Authorization" . ,(wolai-token))))

(wolai--request
 "POST"
 "https://openapi.wolai.com/v1/blocks"
 '((parent_id . "uukdKTgw6VLLuyvgrvE8k3")
   (blocks . [((type . "bookmark")
               (link . "https://www.thepaper.cn/"))]))
 `(("Authorization" . ,(wolai-token))))

(provide 'wolai)
;;; wolai.el ends here
