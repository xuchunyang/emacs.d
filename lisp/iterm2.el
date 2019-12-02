;;; iterm2.el --- Helpers for iTerm2        -*- lexical-binding: t; -*-

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

;; The iTerm2 Python API <https://iterm2.com/python-api/> needs iTerm2 3.3.0+.

;;; Code:

;; https://iterm2.com/python-api/tutorial/running.html#command-line
(defconst iterm2-python (expand-file-name "~/Library/ApplicationSupport/iTerm2/iterm2env/versions/3.7.2/bin/python"))
(defconst iterm2-script (expand-file-name "~/Library/ApplicationSupport/iTerm2/Scripts/emacs.py"))

;;;###autoload
(defun iterm2-send-region (beg end)
  "Send the text of the current region to iTerm2."
  (interactive "r")
  (let ((process-buf (generate-new-buffer " *temp*")))
    (unwind-protect
        (unless (zerop (call-process-region beg end iterm2-python nil process-buf nil iterm2-script))
          (error "`iterm2-send-region' failed: %s"
                 (with-current-buffer process-buf
                   (buffer-string))))
      (when (buffer-live-p process-buf)
        (kill-buffer process-buf)))))

;;;###autoload
(defun iterm2-cd (dir)
  "Change directory to DIR in iTerm2."
  (interactive (list default-directory))
  (setq dir (expand-file-name "."))
  (iterm2-send-region (format "cd '%s'\n" dir) nil))

(provide 'iterm2)
;;; iterm2.el ends here
