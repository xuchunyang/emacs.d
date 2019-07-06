;;; omnifocus.el --- OmniFocus Helpers               -*- lexical-binding: t; -*-

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

;;

;;; Code:

(defun omnifocus-capture (name note)
  "Add task to OmniFocus, NAME as the task name and NOTE as the task note."
  (interactive "sTask name: \nsTask note: ")
  (let ((quote-fn
         (lambda (s)
           "Quote S for passing as a string to AppleScript."
           (mapconcat
            (lambda (char)
              (pcase char
                (?\" (string ?\\ ?\"))
                (?\\ (string ?\\ ?\\))
                (_   (string char))))
            s ""))))
    (do-applescript
     (format
      (concat
       "tell front document of application \"Omnifocus\"\n"
       "  make new inbox task with properties {name:\"%s\", note:\"%s\"}\n"
       "end tell")
      (funcall quote-fn name)
      (funcall quote-fn note)))))

(provide 'omnifocus)
;;; omnifocus.el ends here
