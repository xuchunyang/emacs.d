;; -*- lexical-binding: t; -*-

(defun ls-tree--dir-files (dir)
  (let ((result nil))
    ;; Sort alphabetically like "LC_ALL=C ls"
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (string= (substring file 0 1) ".")
        (setq file (expand-file-name file dir))
        ;; (message "-> %s" file)
        (if (directory-name-p file)
            (push (cons file (ls-tree--dir-files file)) result)
          (push file result))))
    (nreverse result)))

(defun ls-tree--intern (level dir files)
  (let ((idx 0)
        (len (length files))
        prefix)
    (dolist (file files)
      (setq idx (1+ idx))
      (setq prefix (format
                    ;; FIXME: make it work for all levels
                    (cond ((= level 0) "%s-- ")
                          ((= level 1) "|   %s-- ")
                          ((= level 2) "|       %s-- ")
                          ((= level 3) "|           %s-- ")
                          (t (error "TODO xxx")))
                    (if (= idx len) "`" "|")))
      ;; FIXME: Return it as string
      (message "%s"
               (concat prefix
                       (file-relative-name
                        (if (listp file)
                            ;; Remove ending '/'
                            (substring (car file) 0 -1)
                          file)
                        dir)))
      (if (listp file)
          (ls-tree--intern (1+ level) (car file) (cdr file))))))

(defun ls-tree (dir)
  (ls-tree--intern 0 dir (ls-tree--dir-files dir)))

(provide 'ls-tree)
