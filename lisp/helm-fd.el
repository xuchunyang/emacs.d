;;; helm-fd.el --- fd(1) with helm interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/helm-fd
;; Package-Requires: ((emacs "25.1") (helm "0"))
;; Version: 0

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

;; Use fd(1) <https://github.com/sharkdp/fd> with helm interface.

;;; Code:

(require 'helm)

;;;###autoload
(defun helm-fd ()
  (interactive)
  (helm
   :sources
   (helm-build-async-source "fd"
     :header-name (lambda (name)
                    (format "%s in [%s]" name (helm-default-directory)))
     :candidates-process
     (lambda ()
       (let ((process-connection-type nil))
         (let ((proc (apply #'start-process
                            "helm-fd" helm-buffer
                            "fd" (split-string helm-pattern))))
           (set-process-sentinel proc #'ignore)
           proc)))
     :persistent-action 'helm-ff-kill-or-find-buffer-fname    
     :action 'helm-type-file-actions
     :help-message 'helm-generic-file-help-message
     :keymap helm-find-map
     :candidate-number-limit 9999)
   :buffer "*helm fd*"))

(provide 'helm-fd)
;;; helm-fd.el ends here
