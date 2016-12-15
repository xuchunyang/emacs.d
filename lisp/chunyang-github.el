(require 'ghub)
(require 'dash)

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
