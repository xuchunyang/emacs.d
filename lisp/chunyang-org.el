;;; chunyang-org.el --- Personal Extension of Org mode  -*- lexical-binding: nil; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; Keywords: org

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

;; Notes that this file is under lexical binding because
;; `chunyang-org-agenda-csv' requires dynamic binding.

;;; Code:

(require 'org)
(require 'org-agenda)

(defmacro chunyang-org-agenda-csv (cmd-key &rest parameters)
  "Adapted from `org-batch-agenda-csv'."
  (org-eval-in-environment (append '((org-agenda-remove-tags t)
                                     (org-agenda-restore-windows-after-quit t))
				   (org-make-parameter-alist parameters))
    (if (> (length cmd-key) 2)
	(org-tags-view nil cmd-key)
      (org-agenda nil cmd-key)))
  (set-buffer org-agenda-buffer-name)
  (let* ((lines (org-split-string (buffer-string) "\n"))
	 line
         (csv ""))
    (while (setq line (pop lines))
      (catch 'next
	(if (not (get-text-property 0 'org-category line)) (throw 'next nil))
	(setq org-agenda-info
	      (org-fix-agenda-info (text-properties-at 0 line)))
        (setq csv
              (concat
               csv
               (mapconcat 'org-agenda-export-csv-mapper
                          '(org-category txt type todo tags date time extra
                                         priority-letter priority agenda-day)
                          ",")
               "\n"))))
    (let ((org-agenda-restore-windows-after-quit t))
      (org-agenda--quit))
    (substring-no-properties csv)))


;;; Org Easy templates

(require 'helm)

(defun helm-org-easy-templates ()
  "Wrap text in the region or the kill-ring."
  (interactive "*")
  (let ((wrap
         (cond ((use-region-p)
                (let ((beg (region-beginning))
                      (end (region-end)))
                  (prog1 (buffer-substring-no-properties beg end)
                    (delete-region beg end))))
               (kill-ring
                (substring-no-properties (car kill-ring)))
               (t nil))))
    (helm :sources
          (helm-build-sync-source "(org) Easy templates"
            :candidates
            (loop for (key template) in org-structure-template-alist
                  for end = (or (string-match "\n" template) (length template))
                  for disp = (replace-regexp-in-string
                              "\n\n" " ... "
                              (replace-regexp-in-string "\\?" "" template))
                  collect (cons disp (list key template)))
            :action `(lambda (cand)
                       (insert "<" (car cand))
                       (org-try-structure-completion)
                       (when ,wrap
                         (if (= (line-beginning-position)
                                (line-end-position))
                             (insert ,wrap)
                           (when (string-match-p "\n" (cadr cand))
                             (save-excursion
                               (forward-line 1)
                               (when (= (line-beginning-position)
                                        (line-end-position))
                                 (insert ,wrap)))))))))))

;; [[https://emacs-china.org/t/topic/3538/12][针对粘贴代码段的编辑问题 - Emacs-general - Emacs China]]
(defun chunyang-org-format-region-as-code-block (beg end)
  (interactive "*r")
  (let ((lang (read-string "Language: "))
        (ind (save-excursion
               (goto-char beg)
               (back-to-indentation)
               (buffer-substring (line-beginning-position) (point))))
        (code (delete-and-extract-region beg end)))
    (insert ind "#+BEGIN_SRC " lang "\n"
            code (if (string-suffix-p "\n" code) "" "\n")
            ind "#+END_SRC\n")))

(provide 'chunyang-org)
;;; chunyang-org.el ends here
