;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor"))

;; vendor function for loading vendor libs and customizations
(load "custom/vendor.el")

;; packages to load via elpa/marmalade (see custom/package.el)
(defvar lliles-packages (list 'idle-highlight
                              'ruby-mode
                              'inf-ruby
                              'css-mode
                              'find-file-in-project
                              'magit
                              'gist
                              'paredit
                              'htmlize
                              'http-twiddle
                              'ruby-electric))

;; loads vendor package from vendor/ and init/customizations from
;; custom/
(vendor 'package)
(vendor 'espresso)
(vendor 'javadoc-help)
(vendor 'ri)

(load "custom/settings.el")
(load "custom/defuns.el")
(load "custom/bindings.el")
(load "custom/registers.el")
(load "custom/lisp.el")
(load "custom/js.el")

