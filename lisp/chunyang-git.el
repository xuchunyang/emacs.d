;;; chunyang-git.el --- Git support  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

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

(require 'magit)
(require 'hierarchy)


;;; Git Tree

;; [[https://emacs-china.org/t/topic/4627][以树状图展示 Git 项目 - Emacs-general - Emacs China]]

;; Make a new button type because the default 'button face has a
;; underline and I don't like that.
(define-button-type 'chunyang-git-tree
  'follow-link t                        ; Click via mouse
  'face 'default)

;;;###autoload
(defun chunyang-git-tree ()
  (interactive)
  (unless (magit-toplevel)
    (user-error "Not a Git repository"))
  (let* ((topdir (magit-toplevel))
         (buffer-name
          (concat "*git-tree " (abbreviate-file-name topdir))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let* ((default-directory topdir)
             (files (magit-tracked-files))
             (hierarchy (hierarchy-new)))
        ;; Fill the hierarchy
        (hierarchy-add-trees
         hierarchy
         ;; Set . as the root since tree-widget.el requires only one
         ;; root
         (mapcar (lambda (f) (concat "./" f)) files)
         (lambda (f)
           "Return parent directory of F."
           (if (directory-name-p f)
               (file-name-directory (directory-file-name f))
             (file-name-directory f))))
        (hierarchy-sort hierarchy)
        ;; Draw the hierarchy
        (switch-to-buffer
         (hierarchy-tree-display
          hierarchy
          (lambda (f _)
            (insert-text-button
             ;; Basename
             (if (directory-name-p f)
                 (file-name-nondirectory (directory-file-name f))
               (file-name-nondirectory f))
             :type 'chunyang-git-tree
             'action (lambda (_) (find-file f))))
          (get-buffer-create buffer-name)))
        ;; Unfold
        (goto-char (point-min))
        (while (progn (widget-button-press (point))
                      (widget-forward 1)
                      (/= (point) (point-min))))
        (message nil)))))

(provide 'chunyang-git)
;;; chunyang-git.el ends here
