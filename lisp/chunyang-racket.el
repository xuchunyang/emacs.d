;;; chunyang-racket.el --- Racket Utilities          -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
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

(require 'helm)

(defvar racket-search-manual.rkt
  (expand-file-name
   "racket-search-manual.rkt"
   (file-name-directory
    (or load-file-name buffer-file-name))))

(defvar chunyang-racket-search-manual-buffer
  (with-current-buffer (get-buffer-create " *chunyang-racket-search-manual-buffer*")
    (erase-buffer)
    (call-process "racket" nil t nil racket-search-manual.rkt)
    (current-buffer)))

(defvar chunyang-racket-search-manual-helm-source
  (helm-build-in-buffer-source "Search Racket Manuals"
    :data chunyang-racket-search-manual-buffer
    :action (helm-make-actions
             "Help"
             (lambda (candidate)
               (pcase-let* ((`(,id ,from-libs)
                             (split-string
                              candidate
                              " provided from "))
                            (`(,lib . ,_)
                             (split-string
                              from-libs
                              ", ")))
                 ;; racket -e '(help first #:from lazy)'
                 (call-process
                  "racket" nil nil nil "--eval" (format "(help %s #:from %s)" id lib)))))))

(defun chunyang-racket-search-manual ()
  (interactive)
  (helm :sources chunyang-racket-search-manual-helm-source
        :buffer "*helm racket search manual*"))

(provide 'chunyang-racket)
;;; chunyang-racket.el ends here
