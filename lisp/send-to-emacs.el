(require 'web-server)

(defun send-to-emacs-handler (req)
  (pcase-let (((eieio process headers body) req))
    ;; (message "[DEBUG] HEADERS: %S" headers)
    ;; (message "[DEBUG] BODY: %S" body)
    (let ((code 200)
          (body "OK"))
      (if-let ((message (assoc-default "message" headers)))
          (insert
           (decode-coding-string message 'utf-8))
        (setq code 400
              body "Missing message argument"))
      (ws-response-header
       process code
       (cons "Content-Type" "text/plain; charset=utf-8")
       (cons "Content-Length" (string-bytes body)))
      (ws-send process body))))

(defvar send-to-emacs-server nil)

(defun send-to-emacs-stop ()
  (interactive)
  (when send-to-emacs-server
    (message "Close old server")
    (ws-stop send-to-emacs-server)
    (setq send-to-emacs-server nil)))

(defun send-to-emacs-start ()
  (interactive)
  (send-to-emacs-stop)
  (message "Start server")
  (setq send-to-emacs-server
        (ws-start #'send-to-emacs-handler 7000
                  nil :host "0.0.0.0")))
