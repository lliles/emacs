;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (concat user-emacs-directory "vendor"))

;; load themes early to avoid face snatching
(load "custom/themes.el")

;; packages to load via elpa/marmalade (see custom/package.el)
(defvar lliles-packages (list 'idle-highlight-mode
                              'ruby-mode
                              'inf-ruby
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
                              'durendal
                              'scala-mode
                              'smex
                              'ido-ubiquitous))

;; install packages
(load "custom/package.el")

;; vendor function for loading vendor libs and customizations
(load "custom/vendor.el")

;; load vendor lib from vendor/ and customizations from custom/
(vendor 'extraedit)
(vendor 'javadoc-help)
(vendor 'misc-cmds)
(vendor 'ri)
(vendor 'save-frame-geometry)
(vendor 'yasnippet)
(vendor 'rainbow-mode)
(vendor 'mustache-mode)

;; load custom code
(load "custom/settings.el")
(load "custom/defuns.el")
(load "custom/bindings.el")
(load "custom/registers.el")
(load "custom/lisp.el")
(load "custom/js.el")
(load "custom/hybris.el")
;;(load "custom/flyspell.el")
(load "custom/smex.el")

;; load system specific code
(load (concat "custom/" system-name ".el") 'noerror)

;; use this instance as a server
(server-start)
