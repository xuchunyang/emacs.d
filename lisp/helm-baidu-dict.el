;;; helm-baidu-dict.el --- 用 Helm 查询百度汉语 <https://dict.baidu.com/>  -*- lexical-binding: t; -*-

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

;; API: https://dict.baidu.com/hanyu/ajax/sugs?mainkey=%E7%A0%B4%E5%B8%BD

;;; Code:

(defun helm-baidu-dict-suggest-fetch (query)
  (let* ((url-user-agent (format "%s <%s>" user-full-name user-mail-address))
         (url (format "https://dict.baidu.com/hanyu/ajax/sugs?mainkey=%s"
                      (url-hexify-string query)))
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward "^\r?\n")
      (let-alist (let ((json-array-type 'list))
                   (json-read))
        .data.ret_array))))

(defun helm-baidu-dict-suggest-candidates (&optional query)
  (mapcar
   (lambda (x)
     (let ((type (car (alist-get 'type x))))
       (pcase type
         ((guard (member type '("query" "term" "idiom")))
          (let ((sid (car (alist-get 'sid x)))
                (name (car (alist-get 'name x))))
            (cons name sid)))
         ("poemline"
          (let ((title (car (alist-get 'source_poem x)))
                (body (car (alist-get 'source_poem_body x)))
                (author (car (alist-get 'literature_author x)))
                (sid (car (alist-get 'source_poem_sid x))))
            (cons
             (format "%s (%s)\n%s"
                     title (propertize author 'face font-lock-comment-face)
                     (propertize body 'face font-lock-comment-face))
             sid)))
         ("poem"
          (let ((title (car (alist-get 'display_name x)))
                (body (car (alist-get 'body x)))
                (author (car (alist-get 'literature_author x)))
                (sid (car (alist-get 'sid x))))
            (cons
             (format "%s (%s)\n%s"
                     title (propertize author 'face font-lock-comment-face)
                     (propertize body 'face font-lock-comment-face))
             sid)))
         (_ (warn "WARNING: 不支持的类型 %S" x)
            nil))))
   (helm-baidu-dict-suggest-fetch (or query helm-pattern))))

;;;###autoload
(defun helm-baidu-dict-suggest ()
  "百度汉语搜索补全.
URL `https://dict.baidu.com/'."
  (interactive)
  (helm
   :sources
   (helm-build-sync-source "百度汉语"
     :header-name
     (lambda (name)
       (format "%s <%s>" name "https://dict.baidu.com/"))
     :candidates
     #'helm-baidu-dict-suggest-candidates
     :action
     ;; `lexical-binding' must be on
     (let ((URL "https://dict.baidu.com/shici/detail?pid=%s"))
       (helm-make-actions
        "Browse URL"
        (lambda (sid) (browse-url (format URL sid)))
        "EWW URL"
        (lambda (sid) (eww (format URL sid)))))
     :volatile t
     :multiline t
     :requires-pattern 1)
   :full-frame t
   :buffer "*helm 百度汉语*"))

(provide 'helm-baidu-dict)
;;; helm-baidu-dict.el ends here
