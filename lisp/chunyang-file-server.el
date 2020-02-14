;;; chunyang-file-server.el --- Upload File          -*- lexical-binding: t; -*-

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

(require 'web-server)

(defvar chunyang-file-server-for-upload-port 9008)

;; for test, curl -v -F file=@google.png localhost:9008
;; to kill the server, use M-x list-processes
;;;###autoload
(defun chunyang-file-server-for-upload ()
  (interactive)
  (ws-start
   '(((:GET . ".*") .
      (lambda (request)
        (with-slots (process headers) request
          (ws-response-header proc 200 '("Content-type" . "text/html"))
          (process-send-string
           process
           "\
<meta name='viewport' content='width=device-width, initial-scale=1'>
<h1>Upload File</h1>
<form method='post' enctype='multipart/form-data'>
  <input type='file' name='file'>
  <input type='submit'>
</form>
"))))
     ((:POST . ".*") .
      (lambda (request)
        (with-slots (process headers) request
          (let-alist (assoc-default "file" headers)
            (let ((out (make-temp-file "x-" nil .filename)))
              (let ((coding-system-for-write 'binary))
                (write-region .content nil out))
              (message "[%s] saved %d bytes to %s"
                       (current-time-string)
                       (string-bytes .content)
                       out)
              (ws-response-header process 200 '("Content-type" . "text/plain"))
              (process-send-string process (format "saved to %s\n" out))))))))
   chunyang-file-server-for-upload-port nil :host "0.0.0.0")
  (message "Visit %s to upload file to this computer"
           (propertize (format "http://0.0.0.0:%d"
                               chunyang-file-server-for-upload-port)
                       'face 'link)))

;; IDEA: file list server for downloading
;; (info "(web-server) File Server")

(provide 'chunyang-file-server)
;;; chunyang-file-server.el ends here
