;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;            -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path - everything custom in lliles subdirectory
(add-to-list 'load-path (concat user-emacs-directory "lliles/"))
(add-to-list 'load-path (concat user-emacs-directory "lliles/vendor"))

;; TODO is this still necessary?
;; load themes early to avoid face snatching
(load "custom/themes")

;; setup package
(require 'package)
(setq package-user-dir (concat user-emacs-directory "lliles/packages"))
(setq package-enable-at-startup nil)

;; package sources
(dolist 
  (source '(("marmalade" . "https://marmalade-repo.org/packages/")
            ("melpa"     . "https://melpa.org/packages/")))
  (add-to-list 'package-archives source))

(package-initialize)

;; setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package idle-highlight-mode :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package paredit
  :ensure t
  :config
  (defun turn-on-paredit ()
    (paredit-mode t))

  (defun paredit-nonlisp ()
    "Turn on paredit mode for non-lisps."
    (interactive)
    (set (make-local-variable 'paredit-space-for-delimiter-predicates)
         '((lambda (endp delimiter) nil)))
    (paredit-mode 1)))

(use-package clojure-mode :ensure t)

(use-package cider :ensure t)

(use-package rainbow-mode :ensure t)

(use-package yasnippet :ensure t)

(use-package css-mode
  :ensure t
  :mode ("\\.css$"))

(use-package groovy-mode :ensure t)

(use-package paren-face
  :ensure t
  :config
  ;; dim paren faces everywhere
  (global-paren-face-mode 1))

(use-package company
  :ensure t
  :config
  ;; enable company mode everywhere
  (add-hook 'after-init-hook 'global-company-mode))


(use-package exec-path-from-shell
  :ensure t
  :if window-system
  :config
  (exec-path-from-shell-initialize))

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t) 
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-m" . counsel-M-x)))

;; useful stuff i've found that's not available in the repos
(use-package extraedit :load-path "lliles/vendor")
(use-package misc-cmds :load-path "lliles/vendor")
(use-package save-frame-geometry :load-path "lliles/vendor")
(use-package randomize-region :load-path "lliles/vendor")

;; load custom code
(load "custom/settings")
(load "custom/defuns")
(load "custom/bindings")
(load "custom/lisp")
(load "custom/hybris")
(load "custom/flyspell")
(load "custom/ruby")
(load "custom/c")

;; load system specific code
(load (concat "custom/" system-name) 'noerror)

;; use this instance as a server
(server-start)

