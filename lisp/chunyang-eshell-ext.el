(defun eshell-clear-buffer ()
  "Clear terminal"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun eshell/mcd (dir)
  "make a directory and cd into it"
  (eshell/mkdir "-p" dir))

(defun eshell/imgcat (&rest args)
  "Display image(s)."
  (let ((elems (eshell-flatten-list args)))
    (while elems
      (eshell-printn
       (propertize " " 'display (create-image (expand-file-name (car elems)))))
      (setq elems (cdr elems)))))

;; Eshell command name completion for tldr man pages <http://tldr-pages.github.io>
(defvar tldr-commands nil)

(defun pcomplete/tldr ()
  (unless tldr-commands
    (setq tldr-commands
          (split-string
           (nth 1 (split-string
                   (shell-command-to-string "tldr --list")
                   "\n" t))
           ", ")))
  (pcomplete-here* tldr-commands))
