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

(provide 'chunyang-org)
;;; chunyang-org.el ends here
