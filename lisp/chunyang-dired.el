;;; chunyang-dired.el --- Dired hacks  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Xu Chunyang

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Like `find-dired' but works for mdfind, git ls-files etc.

;;; Code:

(defun git-ls-files-dired (dir)
  (interactive
   (list (read-directory-name "Run git ls-files in directory: "
                              nil "" t)))

  (setq dir (file-name-as-directory (expand-file-name dir)))
  (switch-to-buffer (get-buffer-create "*Git ls-files*"))

  (widen)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq default-directory dir)

  (shell-command "git ls-files | xargs ls -l" (current-buffer))

  (dired-mode dir "-l")
  (setq-local dired-sort-inhibit t)
  (setq-local revert-buffer-function `(lambda (ignore-auto noconfirm)
                                        (git-ls-files-dired ,dir)))
  (setq-local dired-subdir-alist
              (list (cons default-directory (point-min-marker))))
  (setq buffer-read-only nil)
  (insert "  " dir ":\n")
  (setq buffer-read-only t))

(provide 'chunyang-dired)
;;; chunyang-dired.el ends here
