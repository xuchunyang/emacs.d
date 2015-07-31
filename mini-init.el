(package-initialize)

;;; OS X
(when (and (window-system) (eq system-type 'darwin))
  (setq mac-command-modifier 'meta
        mac-option-modifier 'control))
;;; Font
(when (window-system) (member "Source Code Pro for Powerline" (font-family-list))
      (set-face-attribute 'default nil :font "Source Code Pro for Powerline 13"))

(add-to-list 'load-path "~/wip/swiper/")
(require 'swiper)

(setq ivy-use-virtual-buffers t)
(ivy-mode)

(require 'counsel)
(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "M-l") #'ivy-switch-buffer)
(global-set-key (kbd "C-x f") #'ivy-recentf)
