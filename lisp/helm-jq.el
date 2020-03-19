;;; helm-jq.el --- Jq on the fly                     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Keywords: tools
;; Package-Requires: ((helm "1.7.9"))

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

;; https://github.com/200ok-ch/counsel-jq

;;; Code:

(require 'helm)

(defun helm-jq-show (_candidate)
  (let ((s (with-helm-buffer
             (save-excursion
               (goto-char (point-min))
               (forward-line 1)
               (buffer-substring (point) (point-max))))))
    (with-current-buffer (get-buffer-create "*jq*")
      (erase-buffer)
      (insert s)
      (goto-char (point-min))
      (select-window (display-buffer (current-buffer))))))

(defun helm-jq (b e)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((json (buffer-substring-no-properties b e))
         (temp (make-temp-file "jq" nil nil json)))
    (unwind-protect
        (helm :sources
              (helm-build-async-source "jq"
                :candidates-process
                (lambda ()
                  (let ((proc (make-process
                               :name "jq"
                               :buffer helm-buffer
                               :command (list "jq" "-M" helm-pattern temp)
                               :connection-type 'pipe)))
                    (set-process-sentinel proc #'ignore)
                    proc))
                :action (helm-make-actions "Show result" #'helm-jq-show)
                :nohighlight t)
              :buffer "*helm jq*")
      (delete-file temp))))

(provide 'helm-jq)
;;; helm-jq.el ends here
