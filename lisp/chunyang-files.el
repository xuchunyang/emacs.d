;;; chunyang-files.el --- File utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xu.chunyang@icloud.com>
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

;; Base on code from https://gihub.com/lunaryorn/.emacs.d

;;; Code:


;;; Working with the current file
;;;###autoload
(defun lunaryorn-rename-file-and-buffer ()
  "Rename the current file and buffer."
  (interactive)
  (let* ((filename (buffer-file-name))
         (old-name (if filename
                       (file-name-nondirectory filename)
                     (buffer-name)))
         (new-name (read-file-name "New name: " nil nil nil old-name)))
    (cond
     ((not (and filename (file-exists-p filename))) (rename-buffer new-name))
     ((vc-backend filename) (vc-rename-file filename new-name))
     (t
      (rename-file filename new-name 'force-overwrite)
      (set-visited-file-name new-name 'no-query 'along-with-file)))))

;;;###autoload
(defun lunaryorn-delete-file-and-buffer ()
  "Delete the current file and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (cond
     ((not filename) (kill-buffer))
     ((vc-backend filename) (vc-delete-file filename))
     (t
      (delete-file filename)
      (kill-buffer)))))


;;; Init file and packages

;;;###autoload
(defun lunaryorn-recompile-packages ()  ; NOTE: I don't know when to use this
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

(defun lunaryorn-all-init-files (&optional with-packages)
  "Return a list of all Emacs Lisp files in my configuration.

If WITH-PACKAGES is given and non-nil include 3rd party
packages."
  (append (list user-init-file)
          (directory-files-recursively (locate-user-emacs-file "lisp/")
                                       (rx ".el" eos))
          (if with-packages
              (directory-files-recursively package-user-dir
                                           (rx ".el" eos))
            nil)))

;;;###autoload
(defun lunaryorn-count-config-lines (&optional with-packages)
  "Show a buffer with LoC statistics for my Emacs config.

If WITH-PACKAGES is given and non-nil include 3rd party packages
into the count."
  (interactive "P")
  (let ((cloc (executable-find "cloc")))
    (unless cloc
      (user-error "Please install cloc"))
    (with-current-buffer (get-buffer-create " *LoC Emacs configuration*")
      (text-mode)
      (read-only-mode)
      (view-mode)
      (let ((inhibit-read-only t)
            (files (lunaryorn-all-init-files with-packages)))
        (erase-buffer)
        (goto-char (point-min))
        (apply #'call-process cloc nil t t "--quiet" files))
      (pop-to-buffer (current-buffer)))))

(provide 'chunyang-files)
;;; chunyang-files.el ends here
