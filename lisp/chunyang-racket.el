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

(defvar racket-search-manual-cache-file
  (make-temp-file "racket-search-manual-cache-file-"))

(defun racket-search-manual-cache-file-write ()
  (with-temp-buffer
    (unless (zerop (call-process "racket" nil t nil racket-search-manual.rkt))
      (error "%s" (buffer-string)))
    (write-region nil nil racket-search-manual-cache-file)))

(defvar chunyang-racket-search-manual-actions
  (helm-make-actions
   "Open Help in Browser"
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
        "racket" nil nil nil "--eval" (format "(help %s #:from %s)" id lib))))))

(defvar chunyang-racket-search-manual-source nil)

(defun chunyang-racket-search-manual (update)
  "Search Racket Manual, with prefix arg, update the cache."
  (interactive "P")
  (when (or update (not (file-exists-p racket-search-manual-cache-file)))
    (message "[chunyang-racket-search-manual] Creating cache...")
    (racket-search-manual-cache-file-write))
  (unless chunyang-racket-search-manual-source
    (setq chunyang-racket-search-manual-source
          (helm-build-in-file-source
              "Search Racket Manuals" racket-search-manual-cache-file
            :action chunyang-racket-search-manual-actions)))
  (helm :sources chunyang-racket-search-manual-source
        :buffer "*helm racket search manual*"))

(provide 'chunyang-racket)
;;; chunyang-racket.el ends here
