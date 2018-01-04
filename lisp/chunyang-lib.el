;;; chunyang-lib.el --- Library of Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: algorithm

;;; Commentary:

;;

;;; Code:


;;; Shuffle

;; TODO Support Sequence, not just List, and try to do it by extending seq.el
(defun chunyang-list-shuffle (l)
  "Return a shuffled list of L."
  (let* ((l (copy-sequence l))
         (i (length l))
         j)
    (while (> i 1)
      (setq i (- i 1))
      (setq j (random i))
      (cl-rotatef (nth i l) (nth j l)))
    l))


;;; Run length

;; https://en.wikipedia.org/wiki/Run-length_encoding

(defun chunyang-run-length-encode (str)
  "Return Run-Length representation of STR as a string."
  (with-temp-buffer
    (let ((idx 0)
          (len (length str))
          last-char occur)
      (while (< idx len)
        (let ((this-char (aref str idx)))
          (if (eq this-char last-char)
              (setq occur (+ 1 occur))
            (if last-char
                (insert (format "%d%c" occur last-char)))
            (setq last-char this-char
                  occur 1))
          (if (= (+ 1 idx) len)
              (insert (format "%d%c" occur last-char))))
        (setq idx (+ 1 idx))))
    (buffer-string)))

(defun chunyang-run-length-decode (str)
  "Decode Run-Length representation."
  (with-temp-buffer
    (let ((idx 0)
          occur char)
      (while (< idx (length str))
        (let ((this-char (aref str idx)))
          (if (< ?0 this-char ?9)
              (setq occur (concat occur (char-to-string this-char)))
            (setq char this-char)
            (insert (make-string (string-to-number occur) char))
            (setq occur nil)))
        (setq idx (+ 1 idx))))
    (buffer-string)))


;;; Fibonacci number

;; 1 1 2 3 5 8 13 21 34 55 89 144 ...

(defun chunyang-fib (n)
  (if (memq n '(1 2))
      1
    (+ (chunyang-fib (- n 1))
       (chunyang-fib (- n 2)))))

(defun chunyang-fib-list (n)
  (let (last last-last res)
    (dolist (i (number-sequence 1 n))
      (if (memq i '(1 2))
          (setq res (cons 1 res)
                last 1
                last-last 1)
        (let ((this (+ last last-last)))
          (setq res (cons this res)
                last-last last
                last this))))
    (nreverse res)))

(provide 'chunyang-lib)
;;; chunyang-lib.el ends here
