;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "vendor"))

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
                              'ruby-electric
                              'slime
                              'slime-repl
                              'clojure-mode
                              'clojure-test-mode
                              'durendal))

;; install packages
(load "custom/package.el")

;; vendor function for loading vendor libs and customizations
(load "custom/vendor.el")

;; load vendor lib from vendor/ and customizations from custom/
(vendor 'color-theme)
(vendor 'espresso)
(vendor 'extraedit)
(vendor 'javadoc-help)
(vendor 'misc-cmds)
(vendor 'ri)
(vendor 'save-frame-geometry)

;; load custom code
(load "custom/settings.el")
(load "custom/defuns.el")
(load "custom/bindings.el")
(load "custom/registers.el")
(load "custom/lisp.el")
(load "custom/js.el")
(load "custom/hybris.el")
(load "custom/flyspell.el")

;; load system specific code
(load (concat dotfiles-dir "custom/" system-name ".el") 'noerror)

