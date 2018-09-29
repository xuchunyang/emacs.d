;;; expand-emmet.el --- Expand Emmet abbreviation    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

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

;;

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'subr-x)

(defvar url-http-end-of-headers)

(defconst expand-emmet--dir
  (file-name-directory
   (or load-file-name buffer-file-name)))

(defvar expand-emmet--process nil)

(defun expand-emmet--ensure-server-process ()
  "Start the server process if not already."
  (unless (process-live-p expand-emmet--process)
    (setq expand-emmet--process
          (make-process :name "Expand Emmet (Server)"
                        :buffer " *Expand-Emmet (Server)*"
                        :command (list
                                  "node"
                                  (expand-file-name "index.js" expand-emmet--dir))
                        :connection-type 'pipe)))  )

(defun expand-emmet--expand (abbrev)
  (expand-emmet--ensure-server-process)
  (let ((url-request-method "POST")
        (url-request-data abbrev)
        (url-show-status nil))
    (with-current-buffer
        (url-retrieve-synchronously "http://localhost:13850")
      (goto-char (1+ url-http-end-of-headers))
      (json-read))))

;;;###autoload
(defun expand-emmet-line ()
  "Expand Emmet abbrev in the current line."
  (interactive)
  (insert (expand-emmet--expand
           (string-trim
            (delete-and-extract-region (line-beginning-position)
                                       (line-end-position))))))
(provide 'expand-emmet)
;;; expand-emmet.el ends here
