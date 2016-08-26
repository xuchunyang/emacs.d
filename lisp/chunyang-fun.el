;;; chunyang-fun.el --- For fun                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>

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

;;

;;; Code:



;; 尝试模拟电视新闻下方的滚动新闻。

;; abc|    |
;;  ab|c   |
;;   a|bc  |
;;    |abc |
;;    | abc|
;;    |  ab|c
;;    |   a|bc
;;    |    |abc

(defun animate-text (width text)
  (let* ((text-width (length text))
         (left (+ width text-width))
         (align (+ left text-width)))
    (flet ((aux (left)
                (if (zerop left)
                    (list text)
                  (cons (concat (make-string left ?\s) text)
                        (aux (1- left)))))
           (string-pad (s)
                       (let ((padding (make-string (- align (length s)) ?\s)))
                         (concat s padding))))
      (mapcar (lambda (s) (substring s text-width left)) (mapcar #'string-pad (aux left))))))

;; (animate-text 4 "abc")

;; (seq-doseq (s (animate-text 4 "abc"))
;;   (message "%s" s)
;;   (sleep-for 0.5))

(defun foo ()
  (interactive)
  (let ((text
         (buffer-substring (line-beginning-position) (line-end-position))))
    (seq-doseq (s (animate-text (window-width) text))
      (let (message-log-max)
        (message "%s" s)
        (sleep-for 0.1)
        (redisplay)))))

(provide 'chunyang-fun)
;;; chunyang-fun.el ends here
