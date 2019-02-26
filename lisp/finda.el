;;; finda.el --- Finda Emacs Integration             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;; Adapt from ~/.finda/integrations/emacs/finda.el (Copyright 2018 Keming Labs, LLC)

;;; Code:

(require 'json)

(defun finda-get-buffers ()
  (let* ((bs (buffer-list))
         (b (car bs))
         l)
    (while bs
      (unless (string-prefix-p " " (buffer-name b))
        (push `(("name" . ,(buffer-name b))
                ("path" . ,(buffer-file-name b)))
              l))
      (setq bs (cdr bs)
            b (car bs)))
    (setq l (nreverse l))
    (json-encode-list l)))

(defun finda-switch-to-buffer (buffer-name)
  (switch-to-buffer buffer-name))

(provide 'finda)
;;; finda.el ends here
