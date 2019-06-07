;;; github-emojis.el --- GitHub Emojis               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: games

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

;; https://api.github.com/emojis

;;; Code:

(require 'json)
(require 'seq)
(require 'url)

(defvar github-emojis-directory (locate-user-emacs-file "var/github-emojis/")
  "Where to keep Emojis pictures?")

(defvar github-emojis nil)

(defun github-emojis ()
  (unless github-emojis
    (with-current-buffer (url-retrieve-synchronously "https://api.github.com/emojis")
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let ((json-object-type 'alist)
            (json-key-type    'string))
        (setq github-emojis (json-read)))))
  github-emojis)

;; (github-emojis--url-as-filename
;;  "https://github.githubassets.com/images/icons/emoji/unicode/1f631.png?v8")
;; => "1f631.png"
(defun github-emojis--url-as-filename (url)
  (pcase-exhaustive (url-path-and-query (url-generic-parse-url url))
    (`(,path . ,_) (file-name-nondirectory path))))

(defun github-emojis-download (emoji)
  (pcase (assoc emoji (github-emojis))
    ('nil (user-error "Unknow emoji: %s" emoji))
    (`(,_ . ,url)
     (let* ((filename (github-emojis--url-as-filename url))
            (path (expand-file-name filename github-emojis-directory)))
       (unless (file-exists-p path)
         (unless (file-exists-p github-emojis-directory)
           (make-directory github-emojis-directory t))
         (let ((url-show-status nil))
           (url-copy-file url path)))
       path))))

(defun github-emojis-download-all ()
  (let ((progress-reporter (make-progress-reporter
                            "Downloading GitHub Emojis..."
                            0 (length (github-emojis)))))
    (seq-mapn (lambda (emoji index)
                (github-emojis-download (car emoji))
                (progress-reporter-update progress-reporter index))
              (github-emojis)
              (number-sequence 0 (1- (length (github-emojis)))))
    (progress-reporter-done progress-reporter)))

(defun github-emoji (emoji)
  "View GitHub EMOJI."
  (interactive
   (let* ((octocat (with-temp-buffer
                     (insert-image-file (github-emojis-download "octocat"))
                     (buffer-string)))
          (prompt (format "%s GitHub Emoji: " octocat)))
     (list (completing-read prompt (github-emojis) nil t))))
  (find-file (github-emojis-download emoji)))

(provide 'github-emojis)
;;; github-emojis.el ends here
