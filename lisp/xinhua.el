;;; xinhua.el --- 《新华字典》客户端                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: Chinese Dictionary

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

;; 《新华字典》客户端
;;
;; 数据来源:  https://github.com/pwxcoo/chinese-xinhua

;;; Code:

(require 'json)

(defvar url-http-end-of-headers)

(defun xinhua-get-json (word)
  (let ((url (concat "https://www.pwxcoo.com/dictionary?type=word&word=" word)))
    (with-current-buffer (url-retrieve-synchronously url)
      (set-buffer-multibyte t)
      (goto-char (1+ url-http-end-of-headers))
      (json-read))))

;;;###autoload
(defun xinhua (word)
  "显示汉字 WORD 在《新华字典》中的解释."
  (interactive "sWord: ")
  (let ((json (xinhua-get-json word)))
    (when (equal json [])
      (error "No result for %s" word))
    (let-alist (aref json 0)
      (with-current-buffer (get-buffer-create "*Xinhua Zidian*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert .word ?\n
                .pinyin ?\n
                .radicals ?\n
                .explanation ?\n
                .more)
        (goto-char (point-min))
        (setq buffer-read-only t)
        (display-buffer (current-buffer))))))

(provide 'xinhua)
;;; xinhua.el ends here
