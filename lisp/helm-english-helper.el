;;; helm-english-helper.el --- Insert English words with Helm  -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; https://github.com/manateelazycat/company-english-helper
(require 'company-english-helper-data "~/src/company-english-helper/company-english-helper-data.el") ; `english-helper-completions'

(require 'helm)

(defvar helm-english-helper-action
  (helm-make-actions
   "Insert Word"
   (lambda (s) (insert (car (split-string s))))))

(defun helm-english-helper-init ()
  (unless (helm-candidate-buffer)
    (message "[%s] helm-english-helper-init: Fill candidates..." (current-time-string))
    (helm-init-candidates-in-buffer 'global
      (mapcar
       (lambda (s)
         (format "%-25s %s" s (get-text-property 0 :initials s)))
       (seq-filter
        (lambda (s)
          ;; Remove a, an, 'a, -al and 'a bad life'
          (and (> (length s) 2)
               (string-match "^[a-zA-Z]" s)
               (not (string-match " " s))))
        english-helper-completions)))))

(defvar helm-english-helper-source
  (helm-build-in-buffer-source "English words"
    :init #'helm-english-helper-init
    :action 'helm-english-helper-action))

;;;###autoload
(defun helm-english-helper ()
  (interactive)
  (helm :sources helm-english-helper-source
        :buffer "*helm english helper*" ))

(provide 'helm-english-helper)
;;; helm-english-helper.el ends here
