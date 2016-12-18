;;; chunyang-github.el --- Access GitHub user repos and starred repos  -*- lexical-binding: t; -*-

;; Package-Requires: ((ghub "0") (dash "0") (helm "0")

(require 'ghub)
(require 'dash)
(require 'helm)

;;; Internal Helpers

(defvar chunyang-github-repos nil)
(defvar chunyang-github-stars nil)

(defun chunyang-github-get-repos ()
  (unless chunyang-github-repos
    (setq chunyang-github-repos
          (--keep (cdr (assq 'full_name it))
                  (let ((ghub-unpaginate t))
                    (ghub-get "/user/repos")))))
  chunyang-github-repos)

(defun chunyang-github-get-stars ()
  (unless chunyang-github-stars
    (setq chunyang-github-stars
          (--keep (cdr (assq 'full_name it))
                  (let ((ghub-unpaginate t))
                    (ghub-get "/user/starred")))))
  chunyang-github-stars)

;;; User Commands

(defun chunyang-github-stars/repos-browse-url ()
  (interactive)
  (let ((repo (completing-read "GitHub Stars or Repos: "
                               (append (chunyang-github-get-repos)
                                       (chunyang-github-get-stars))
                               nil t)))
    (browse-url (concat "https://github.com/" repo))))

(defun chunyang-github-stars/repos-copy-clone-url ()
  (interactive)
  (let* ((repo (completing-read "GitHub Stars or Repos: "
                                (append (chunyang-github-get-repos)
                                        (chunyang-github-get-stars))
                                nil t))
         (clone-url (concat "git@github.com:" repo ".git")))
    (kill-new clone-url)
    (message "Killed: %s" clone-url)))

(defvar helm-chunyang-github-actions
  '(("Browse URL"         . (lambda (repo)
                              (browse-url (concat "https://github.com/" repo))))
    ("Copy git-clone URL" . (lambda (repo)
                              (let ((url (concat "git@github.com:" repo ".git")))
                                (kill-new url)
                                (message "Killed: %s" url))))))

(defun helm-chunyang-github-stars ()
  (interactive)
  (helm :sources (helm-build-sync-source "GitHub Stars"
                   :candidates 'chunyang-github-get-stars
                   :action 'helm-chunyang-github-actions)
        :buffer "*helm GitHub Stars*"))

(defun helm-chunyang-github-repos ()
  (interactive)
  (helm :sources (helm-build-sync-source "GitHub Repos"
                   :candidates 'chunyang-github-get-repos
                   :action 'helm-chunyang-github-actions)
        :buffer "*helm GitHub Repos*"))

(provide 'chunyang-github)
;;; chunyang-github.el ends here
