;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; vendor function for loading vendor libs and customizations
(load "custom/vendor.el")

(vendor "package")

(load "custom/settings.el")
(load "custom/defuns.el")
(load "custom/bindings.el")
(load "custom/registers.el")
(load "custom/lisp.el")
(load "custom/js.el")

