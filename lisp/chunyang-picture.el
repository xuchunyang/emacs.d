;;; chunyang-picture.el --- Fetch some picture from Web  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; - Download photo on Bing's homepage <http://www.bing.com/>
;; - Download Honey Select picture from https://www.zodgame.us/forum.php

;;; Code:

;;;###autoload
(defun chunyang-download-bing-picture (&optional directory filename resolution)
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

;;;###autoload
(defun chunyang-about-honey-select (&optional dir)
  "Fetch a picture on Honey Select to DIR and display in *About GNU Emacs*.
Argument nil or omitted means save to `default-directory'."
  (interactive)
  (unless dir
    (setq dir (let ((default "~/Pictures/Honey Select"))
                (if (file-exists-p default)
                    default
                  default-directory))))
  (let ((default-directory dir)
        (process-environment (append '("LC_ALL=C") process-environment))
        buf proc)
    (setq buf (get-buffer-create "*wget*"))
    (setq proc (get-buffer-process buf))
    (when proc (kill-process proc))
    (with-current-buffer buf
      (setq proc (start-process
                  "wget" buf "wget"
                  "--server-response"
                  "--content-disposition"
                  "--no-clobber"
                  "http://signavatar.com/50020_s.gif"))
      (require 'shell)
      (shell-mode)
      (display-buffer buf)
      (set-process-sentinel
       proc
       (lambda (_proc event)
         (cond ((string= event "finished\n")
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (re-search-forward "  Location: \\(.+\\)$")
                    (setq fancy-splash-image
                          (expand-file-name (file-name-nondirectory (match-string 1))))
                    (kill-buffer)
                    (delete-other-windows)))
                (about-emacs)))))
      ;; Use the comint filter for proper handling of carriage motion
      ;; (see `comint-inhibit-carriage-motion'),.
      (set-process-filter proc 'comint-output-filter))))

(provide 'chunyang-picture)
;;; chunyang-picture.el ends here
