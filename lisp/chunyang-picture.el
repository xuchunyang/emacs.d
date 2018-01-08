;;; chunyang-picture.el --- Fetch some picture from Web  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: picture

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

;; - Download photo on Bing's homepage <http://www.bing.com/>

;;; Code:

;;;###autoload
(defun chunyang-picture-bing (&optional directory filename resolution)
  "下载 Bing 首页图片.
用 DIRECTORY 指定下载目录，省略或 nil 用当前路径.
用 FILENAME 指定文件名，省略或 nil 用服务器提供的.
用 RESOLUTION 指定分辨率，如\"1920*1086\"，省略或 nil 用服务器提供的."
  (interactive)
  (let ((api
         "http://www.bing.com/HPImageArchive.aspx?format=js&idx=0&n=1&mkt=zh-CN")
        json pic-url pic-file)
    (if filename
        (setq pic-file (expand-file-name filename directory)))
    (if (and pic-file (file-exists-p pic-file))
        pic-file
      (with-current-buffer (url-retrieve-synchronously api)
        (set-buffer-multibyte t)
        (goto-char (point-min))
        (delete-region (point-min)
                       (1+ (re-search-forward "^$" nil t)))
        (goto-char (point-min))
        (require 'json)
        (setq json (json-read))
        ;; http://bing.com/az/hprichbg/rb/PonteSantAngelo_ZH-CN15413822788_1920x1080.jpg
        (setq pic-url
              (concat
               "http://bing.com"
               (assoc-default 'url (aref (assoc-default 'images json) 0))))
        ;; 默认用一个分辨率较低的，适合 `fancy-splash-image'
        ;; http://bing.com/az/hprichbg/rb/PonteSantAngelo_ZH-CN15413822788_640x360.jpg
        (setq pic-url
              (replace-regexp-in-string
               (rx "_" (1+ digit) "x" (1+ digit) ".jpg" string-end)
               (if resolution
                   (format "_%s.jpg" resolution)
                 "_640x360.jpg")
               pic-url))
        (unless pic-file
          (setq pic-file
                (expand-file-name (file-name-nondirectory pic-url) directory)))
        (unless (file-exists-p pic-file)
          (url-copy-file pic-url pic-file))
        pic-file))))

(provide 'chunyang-picture)
;;; chunyang-picture.el ends here
