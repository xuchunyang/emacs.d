;;; symbolic-link-on-save.el --- Create Symbolic link on save  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
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

;; XXX: Try `firestarter' (Run shell command on save) instead

;;; Code:

(defgroup symbolic-link-on-save nil
  "Create Symbolic Link on save."
  :group 'files)

(defcustom symbolic-link-on-save-linkname nil
  "Symbolic Link name for the current file."
  :type '(choice (const nil) (string))
  :safe 'stringp)
(make-variable-buffer-local 'symbolic-link-on-save-linkname)

(defun symbolic-link-on-save ()
  (when symbolic-link-on-save-linkname
    (let ((linkname (expand-file-name symbolic-link-on-save-linkname))
          (target buffer-file-name))
      ;; In case Emacs is already visiting the symbol link
      (unless (equal (file-symlink-p target) linkname)
        ;; link is already exist
        (if (file-exists-p linkname)
            ;; In case the link is already what we wanted
            (unless (equal (file-symlink-p linkname) target)
              (if (y-or-n-p
                   (format "%s is exist, delete it for creating symbolic link? "
                           linkname))
                  (progn
                    (if delete-by-moving-to-trash
                        (delete-file linkname t)
                      (copy-file linkname (concat linkname ".old") t)
                      (delete-file linkname))
                    ;; link is now deleted, create symbolic link
                    (make-directory (file-name-directory linkname) t)
                    (make-symbolic-link target linkname))
                (user-error "%s is exist, cann't create symbolic link" linkname)))
          ;; link is not exist, create symbolic link
          (make-directory (file-name-directory linkname) t)
          (make-symbolic-link target linkname))))))

;;;###autoload
(define-minor-mode symbolic-link-on-save-mode
  "Create symbolic link on save."
  :global t
  (if symbolic-link-on-save-mode
      (add-hook 'after-save-hook #'symbolic-link-on-save t)
    (remove-hook 'after-save-hook #'symbolic-link-on-save t)))

(provide 'symbolic-link-on-save)
;;; symbolic-link-on-save.el ends here
