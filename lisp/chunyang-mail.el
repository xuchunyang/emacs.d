;;; chunyang-mail.el --- Mail                        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Keywords: mail

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

(require 'dom)
(require 'rx)
(require 'seq)

(defvar url-http-end-of-headers)

;; http://lists.gnu.org/archive/html/
;;
;; - emacs-orgmode
;; - emacs-devel
;; - bug-gnu-emacs
;; - help-gnu-emacs
;;
;; (chunyang-browse-gnu-message "<87lgd671k7.fsf@bzg.fr>" "emacs-orgmode")
(defun chunyang-browse-gnu-message (message-id list)
  (interactive
   (let ((message-id (read-string "Message-ID: "))
         (list (completing-read
                "Mail list (see http://lists.gnu.org/archive/html/ for all lists): "
                '("emacs-orgmode" "emacs-devel"))))
     (list message-id list)))
  (let ((url (concat "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?"
                     (url-build-query-string
                      `(("query"  ,(concat "+message-id:" message-id))
                        ("submit" "Search!")
                        ("idxname" ,list)
                        ("max"    "20")
                        ("result" "normal")
                        ("sort"   "score"))))))
    (with-current-buffer (url-retrieve-synchronously url)
      (let* ((html (libxml-parse-html-region url-http-end-of-headers (point-max)))
             (links (dom-by-tag html 'a))
             (link (seq-find
                    (lambda (link)
                      (let ((target (dom-attr link 'href))
                            (description (dom-text link)))
                        (and target
                             (string-match-p
                              (rx (repeat 4 digit) "-" (repeat 2 digit) "/msg" (repeat 5 digit) ".html")
                              ;; "/archive/html/emacs-orgmode/2018-04/msg00398.html"
                              target))))
                    links))
             (target (when link
                       (dom-attr link 'href)))
             (absolute-target (when target
                                (concat "http://lists.gnu.org/" target))))
        (if link
            (let ((target (dom-attr link 'href))
                  (description (dom-text link)))
              (setq target (concat "http://lists.gnu.org/" target))
              (message "Visit %s (%s)..." target description)
              (browse-url target))
          (message "No match found on %s" url))))))

(provide 'chunyang-mail)
;;; chunyang-mail.el ends here
