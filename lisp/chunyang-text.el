;;; chunyang-text.el --- Some text process utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>
;; URL: https://github.com/xuchunyang/emacs.d
;; Created: 2017-02-24
;; Modified: 2017-02-24

;;; Commentary:

;;

;;; Code:


;;; Wrap text like cowsay(1)

;; Examples:
;;  _______
;; < hello >
;;  -------
;;  _________________________________________
;; / Real Vim ninjas count every keystroke - \
;; \ do you?                                 /
;;  -----------------------------------------
;;  _________________________________________
;; / Real Vim ninjas count every keystroke - \
;; | do you? Real Vim ninjas count every     |
;; \ keystroke - do you? Real Vim ninjas     /
;;  -----------------------------------------
;;  _________________________________________
;; / Real Vim ninjas count every keystroke - \
;; | do you? Real Vim ninjas count every     |
;; | keystroke - do you? Real Vim ninjas     |
;; \ count every keystroke - do you?         /
;;  -----------------------------------------

(defun chunyang-text-wrap-like-cowsay (text)
  "Wrap TEXT like cowsay.
Notes that assuming TEXT is already filled and TEXT should not
include multiple bytes characters."
  (let* ((lines (split-string text "\n"))
         (max-len (max (mapcar #'length lines))))
    ;; TODO ...
    ))



;;; Provide

(provide 'chunyang-text)

;; Local Variables:
;; time-stamp-pattern: "20/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; chunyang-text.el ends here
