;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;            -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path - everything custom in lliles subdirectory
(add-to-list 'load-path (concat user-emacs-directory "lliles/"))

;; load themes early to avoid face snatching
(load "custom/themes")

;; packages to load via marmalade, elpa (see custom/package.el)
(defvar package-install-list '(idle-highlight-mode
                               find-file-in-project
                               magit
                               paredit
                               clojure-mode
                               smex
                               ido-ubiquitous
                               rainbow-mode
                               yasnippet
                               css-mode
                               groovy-mode
                               ))
(load "custom/package")

;; load vendor lib from vendor/ and customizations from custom/
(defvar vendor-install-list '(extraedit
                              misc-cmds
                              save-frame-geometry
                              randomize-region
                              ))
(load "custom/vendor")

;; load custom code
(load "custom/settings")
(load "custom/defuns")
(load "custom/bindings")
(load "custom/aliases")
(load "custom/hooks")
(load "custom/registers")
(load "custom/lisp")
(load "custom/js")
(load "custom/hybris")
(load "custom/flyspell")
(load "custom/ruby")

;; load system specific code
(load (concat "custom/" system-name) 'noerror)

;; use this instance as a server
(server-start)

