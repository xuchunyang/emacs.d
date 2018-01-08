;;; cowsay.el --- Implement cowsay(1) in Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Created: 2017-03-05
;; Modified: 2018-01-09

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

(defun cowsay--string-pad (len string)
  "If STRING is shorter than LEN, pad it with space in the end."
  (format (format "%%-%ds" len) string))

(defun cowsay--trim (string)
  ;; Remove trailing newlines
  (setq string
        (if (string-match "\n+\\'" string)
            (replace-match "" t t string)
          string))
  ;; Replace multiple spaces and/and tabs with one space
  (if (string-match "[ \t]+" string)
      (replace-match " " t t string)
    string))

(defun cowsay--fill (string column)
  "Fill STRING within the max width COLUMN"
  (with-temp-buffer
    (insert string)
    (let ((fill-column column))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun cowsay--wrap (string)
  "Wrap STRING in a box."
  (let* ((strings (split-string string "\n"))
         (lines (length strings))
         (width (apply #'max (mapcar #'length strings))))
    (cond ((= 1 lines)
           (format (concat " %s "   "\n"
                           "< %s >" "\n"
                           " %s "   "\n")
                   (make-string (+ 2 width) ?_)
                   (car strings)
                   (make-string (+ 2 width) ?-)))
          ((= 2 lines)
           (format (concat " %s "   "\n"
                           "/ %s \\" "\n"
                           "\\ %s /" "\n"
                           " %s "   "\n")
                   (make-string (+ 2 width) ?_)
                   (cowsay--string-pad width (car strings))
                   (cowsay--string-pad width (cadr strings))
                   (make-string (+ 2 width) ?-)))
          (t
           (let ((idx 0)
                 (result ""))
             (while (< idx (+ 2 lines))
               (setq result
                     (cond ((= idx 0)
                            (concat result " " (make-string (+ 2 width) ?_) " \n"))
                           ((= idx 1)
                            (concat
                             result
                             "/ " (cowsay--string-pad width (car strings)) " \\\n"))
                           ((= idx (- (+ 2 lines) 2))
                            (concat
                             result
                             "\\ " (cowsay--string-pad width (nth (1- idx) strings)) " /\n"))
                           ((= idx (- (+ 2 lines) 1))
                            (concat result " " (make-string (+ 2 width) ?-) " \n"))
                           (t
                            (concat result "| " (cowsay--string-pad width (nth (1- idx) strings)) " |\n"))))
               (setq idx (1+ idx)))
             result)))))

(defun cowsay (string &optional column message-p)
  "Cowsay STRING within max COLUMN (nil defaults to 40).
Echo the result in echo area if MESSAGE-P is non nil.  When
called interactively, STRING is filled with the text in the
region and MESSAGE-P is always non-nil.

This should be the same as M-| cowsay."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties
         (region-beginning)
         (region-end))
      (user-error "No region")) nil t))
  (let ((cowsay
         (concat (cowsay--wrap (cowsay--fill (cowsay--trim string) (or column 40)))
                 "        \\   ^__^
         \\  (oo)\\_______
            (__)\\       )\\/\\
                ||----w |
                ||     ||")))
    (when message-p
      (message "%s" cowsay))
    cowsay))

(provide 'cowsay)
;;; cowsay.el ends here

;; Local Variables:
;; time-stamp-pattern: "20/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:
