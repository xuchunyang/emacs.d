;;; chunyang-helm-SDL-api.el --- Search SQL API doc  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang <xuchunyang56@gmail.com>
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

;; cache
(defvar chunyang-helm-SDL-api-source nil)

(defvar chunyang-helm-SDL-api-actions
  (helm-make-actions
   "Open in browser"
   (lambda (href)
     (browse-url (concat "https://wiki.libsdl.org/" href)))
   "Open in EWW"
   (lambda (href)
     (eww (concat "https://wiki.libsdl.org/" href)))))

;;;###autoload
(defun chunyang-helm-SDL-api ()
  "Search SDL API."
  (interactive)
  (unless chunyang-helm-SDL-api-source
    (setq chunyang-helm-SDL-api-source
          (helm-build-sync-source "SDL API"
            :candidates (chunyang-helm-SDL-api-fetch)
            :action 'chunyang-helm-SDL-api-actions)))
  (helm :sources chunyang-helm-SDL-api-source
        :buffer "*helm SDL API*"))

(defun chunyang-helm-SDL-api-fetch ()
  (with-current-buffer (url-retrieve-synchronously
                        "https://wiki.libsdl.org/CategoryAPI")
    ;; xpath should be much easier, see sxpath provided from sxml (racket)
    (mapcan
     (lambda (div)
       (mapcar
        (lambda (a)
          (cons (dom-text a)
                (dom-attr a 'href)))
        (dom-by-tag div 'a)))
     (dom-by-class
      (libxml-parse-html-region url-http-end-of-headers (point-max))
      "searchresults"))))

(provide 'chunyang-helm-SDL-api)
;;; chunyang-helm-SDL-api.el ends here
