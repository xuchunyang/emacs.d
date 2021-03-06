(add-to-list
 'org-capture-templates
 '("l" "Link" entry (file "links.org")
   "* %a"
   :immediate-finish t))

(require 'web-server)

(defun chunyang-org-capture-server (request)
  (pcase-let (((eieio process headers) request))
    (message "%S" headers)
    (let ((path (assoc-default :GET headers)))
      ;; 只处理 GET /
      (if (not (and path (string= path "/")))
          (ws-send-404 process)
        (let ((href (assoc-default "href" headers))
              (title (assoc-default "title" headers)))
          (if (not (and href title))
              (ws-send-404 process "Missing href and/or title argument"))
          (setq title (decode-coding-string title 'utf-8))
          (push (list href title) org-stored-links)
          ;; XXX 不确定这对不对
          (org-link-store-props :type "http"
			        :link href
			        :description title
			        :annotation (org-link-make-string href title))
          (let ((org-capture-link-is-already-stored t))
            (org-capture nil "l"))
          (let ((body "
<meta name='viewport' content='width=device-width, initial-scale=1'>
<h3>Successfully sent to Org Capture</h3>"))
            (ws-response-header
             process 200
             (cons "Content-Type" "text/html")
             (cons "Content-Length" (string-bytes body)))
            (ws-send process body)))))))

(ws-start #'chunyang-org-capture-server 3000)
