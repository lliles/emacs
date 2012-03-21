;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;            -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path
(add-to-list 'load-path user-emacs-directory)

;; load themes early to avoid face snatching
(load "custom/themes")

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
                              'scala-mode
                              'smex
                              'ido-ubiquitous
                              'javadoc-help
                              'rainbow-mode))

;; install packages
(load "custom/package")

;; load vendor lib from vendor/ and customizations from custom/
(load "custom/vendor")
(vendor 'extraedit)
(vendor 'misc-cmds)
(vendor 'ri)
(vendor 'save-frame-geometry)
(vendor 'yasnippet)
(vendor 'mustache-mode)

;; load custom code
(load "custom/settings")
(load "custom/defuns")
(load "custom/bindings")
(load "custom/aliases")
(load "custom/registers")
(load "custom/lisp")
(load "custom/js")
(load "custom/hybris")
;;(load "custom/flyspell")
(load "custom/smex")

;; load system specific code
(load (concat "custom/" system-name) 'noerror)

;; use this instance as a server
(server-start)
