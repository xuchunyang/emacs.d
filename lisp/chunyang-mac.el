;;; chunyang-mac.el --- macOS Supports  -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017  Chunyang Xu

;; Author: Chunyang Xu <mail@xuchunyang.me>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some macOS supports

;;; Code:


;;; Working with Terminal.app

(defun chunyang-mac-escape-quote (s)
  "Convert \" in S into \\\"."
  (replace-regexp-in-string "\"" "\\\\\"" s))

(defun chunyang-mac-Terminal-send-string (s)
  "Run STR in Terminal.app."
  (do-applescript
   (format (concat
            "tell application \"Terminal\"\n"
            "activate\n"
            "do script \"%s\" in window 1\n"
            "end tell")
           (chunyang-mac-escape-quote s))))

(defun chunyang-mac-Terminal-send-region (start end)
  "Send the current region to Terminal.app."
  (interactive "r")
  (chunyang-mac-Terminal-send-string (buffer-substring start end)))

(defun chunyang-mac-Terminal-cd (dir)
  "Open Terminal.app and cd to DIR in it."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  (chunyang-mac-Terminal-send-string (format "cd '%s'" dir)))


;;; Finder.app

;; IDEA: Reveal multiple files
(defun chunyang-mac-Finder-reveal (file)
  "Reveal (select/highlight) FILE in Finder."
  (interactive (list (or (buffer-file-name) ".")))
  ;; FIXME: It is better and easier to use 'open -R'
  (do-applescript
   (format (concat
            "tell application \"Finder\"\n"
            "	activate\n"
            "	reveal POSIX file \"%s\"\n"
            "end tell")
           (expand-file-name file))))


;;; Tags

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))

(defun chunyang-mac-edit-file-tags (file)
  "Edit the macOS file Tags of FILE.

For usage of the macOS file Tags, see 
URL `https://support.apple.com/kb/PH25325?locale=en_US'."
  (interactive
   (let* ((default (cond ((eq major-mode 'dired-mode)
                          (dired-get-filename nil t))
                         (t (thing-at-point 'filename))))
          (prompt (if default
                      (format "File (default %s): " default)
                    "File: "))
          (file (read-file-name prompt nil default)))
     (list file)))
  (unless (eq system-type 'darwin)
    (user-error "The OS '%s' is not supported" system-type))
  (unless (executable-find "tag")
    (user-error "The program 'tag' is not found"))
  (let* ((old-tags
          (with-temp-buffer
            (call-process "tag" nil t nil "--no-name" file)
            (goto-char (point-min))
            (buffer-substring (line-beginning-position) (line-end-position))))
         (new-tags
          (read-string
           (format "Set Tags of '%s': "
                   (abbreviate-file-name file))
           old-tags)))
    (call-process "tag" nil nil nil "--set" new-tags file)))

(provide 'chunyang-mac)
;;; chunyang-mac.el ends here
