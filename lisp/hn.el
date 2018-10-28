;;; hn.el --- List Hacker News                       -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Version: 2018.10.28
;; Package-Requires: ((emacs "26"))
;; Homepage: https://github.com/xuchunyang/emacs.d/
;; Keywords: Hacker News

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; List Hacker News in Emacs via M-x list-hacker-news

;;; Code:

(require 'dom)

(defvar url-http-end-of-headers)

(defun hn-get-links ()
  (let ((dom (with-current-buffer (url-retrieve-synchronously
                                   "https://news.ycombinator.com/")
               (libxml-parse-html-region url-http-end-of-headers (point-max)))))
    (mapcar (lambda (a)
              (list :title (dom-text a)
                    :href (dom-attr a 'href)))
            (dom-by-class dom "storylink"))))

(defun hn-insert-link (link)
  "Insert LINK."
  (insert
   (propertize (plist-get link :title)
               'mouse-face 'highlight
               'help-echo (concat "mouse-2: visit " (plist-get link :href))
               'follow-link t
               'keymap
               (let ((map (make-sparse-keymap))
                     (cmd (lambda ()
                            (interactive)
                            (browse-url (plist-get link :href)))))
                 (define-key map [mouse-2] cmd)
                 (define-key map [?\C-m] cmd)
                 map))
   ?\n))

(defalias 'list-hacker-news 'hn-list)

(defun hn-list ()
  "List Hacker News."
  (interactive)
  (with-current-buffer (get-buffer-create "*Hacker News*")
    (setq display-line-numbers t)
    (setq buffer-read-only nil)
    (erase-buffer)
    (mapc #'hn-insert-link (hn-get-links))
    (delete-char -1)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(provide 'hn)
;;; hn.el ends here
