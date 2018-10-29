;;; currency-converter.el --- Currency Converter     -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Package-Requires: ((emacs "25"))
;; Version: 2018.10.29
;; Homepage: https://github.com/xuchunyang/emacs.d/

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Currency Converter

;;; Code:

(require 'dom)
(require 'seq)

(defvar url-http-end-of-headers)

(defvar currency-converter-xe-rate
  (prog1 nil
    ;; Sample
    '(("USD" "US Dollar" "1.0000000000" "1.0000000000")
      ("CNY" "Chinese Yuan Renminbi" "6.9581014065" "0.1437173651")
      ...)))

(defun currency-converter-xe-rate ()
  (unless currency-converter-xe-rate
    (let* ((dom
            (with-current-buffer
                (url-retrieve-synchronously
                 ;; https://www.xe.com/currencytables/?from=USD&date=2018-10-29
                 (concat "https://www.xe.com/currencytables/?from=USD&date="
                         (format-time-string "%Y-%m-%d")))
              (libxml-parse-html-region url-http-end-of-headers (point-max))))
           (table (dom-by-id dom "historicalRateTbl"))
           (tds (dom-by-tag table 'td))
           (texts (mapcar #'dom-texts tds))
           (rates (seq-partition texts 4)))
      (setq currency-converter-xe-rate rates))))

(defun currency-converter-USD (name)
  (string-to-number (car (last (assoc name currency-converter-xe-rate)))))

(defun currency-converter (amount from to)
  "Convert AMOUNT currency from FROM to TO."
  (* amount
     (/ (currency-converter-USD from)
        (currency-converter-USD to))))

;; (currency-converter 1 "CNY" "JPY")
;; => 16.12944240557397

(provide 'currency-converter)
;;; currency-converter.el ends here
