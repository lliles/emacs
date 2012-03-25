(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))

;; solarized
;(let ((solarized-italic nil)
;      (solarized-termcolors 256))
;  (load-theme 'solarized-dark t))

;; zenburn
;(load-theme 'zenburn t)

;; dark-laptop
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/dark-laptop"))
(load-theme 'dark-laptop t)
