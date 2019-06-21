;;; company-google-translate.el --- Company backend for Google Translate  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/emacs.d/lisp/company-google-translate.el
;; Package-Requires: ((emacs "25.1") (company "0") (google-translate "0"))
;; Keywords: i18n
;; Version: 0

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

;; Use Google Translate with Company

;;; Code:

(require 'google-translate)
(require 'company)
(require 'seq)
(require 'subr-x)

(defun company-google-translate--candidates (chinese)
  (let ((json (let ((url-show-status nil))
                (google-translate-request "zh-CN" "en" chinese))))
    (cons
     (google-translate-json-translation json)
     (seq-mapcat
      (lambda (item)
        (mapcar
         (lambda (trans)
           (propertize
            ;; 英文单词
            (aref trans 0)
            'translate
            ;; 中文翻译
            (string-join (aref trans 1) ", ")
            'type
            ;; 词性，名词、动词、副词、形容词
            (aref item 0)
            ))
         (aref item 2)))
      (google-translate-json-detailed-translation json)))))

;;;###autoload
(defun company-google-translate (command &optional arg &rest _ignored)
  (interactive (list 'interactive))
  (pcase command
    ('interactive (company-begin-backend 'company-google-translate))
    ('prefix (let ((word (company-grab-word)))
               ;; 只考虑中文
               (when (string-match-p (rx bos (1+ (category chinese)) eos) word)
                 word)))
    ('candidates (company-google-translate--candidates arg))
    ('annotation (when (get-text-property 0 'translate arg)
                   (format "%s [%s]"
                           (get-text-property 0 'translate arg)
                           (substring (get-text-property 0 'type arg) 0 1))))
    ;; Google 返回的结果按 Frequency 排序，保留它
    ('sorted t)
    ('no-cache t)))

(provide 'company-google-translate)
;;; company-google-translate.el ends here
