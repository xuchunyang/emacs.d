(defun dmenu-completing-read (prompt strings)
  (let ((temp-stdin-buffer
         (generate-new-buffer " *temp-stdin"))
        (temp-stdout-buffer
         (generate-new-buffer " *temp-stdout")))
    (unwind-protect
        (progn
          (with-current-buffer temp-stdin-buffer
            (dolist (s strings)
              (insert s ?\n))
            (shell-command-on-region (point-min)
                                     (point-max)
                                     (format "dmenu -p '%s'" prompt)
                                     temp-stdout-buffer)
            (with-current-buffer temp-stdout-buffer
              (buffer-substring-no-properties (point-min)
                                              (line-end-position)))))
      (kill-buffer temp-stdin-buffer)
      (kill-buffer temp-stdout-buffer))))

(defun dmenu-M-x (prefix command)
  (interactive
   (list current-prefix-arg
         (intern
          (dmenu-completing-read
           "M-x"
           (all-completions "" obarray #'commandp)))))
  (and (commandp command)               ; Prevent fail of dmenu
       (command-execute command)))

(provide 'dmenu)
