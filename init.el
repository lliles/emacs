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

;; setup all the rest
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

(use-package browse-url
  :config
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/lliles/packages/.*\\'" ; Package files
                              )))

(use-package paren
  :config
  (show-paren-mode 1))


;; setup stuff not found in repos
(use-package extraedit :load-path "lliles/vendor")
(use-package misc-cmds :load-path "lliles/vendor")
(use-package save-frame-geometry :load-path "lliles/vendor")
(use-package randomize-region :load-path "lliles/vendor")

(setq visible-bell nil
      ring-bell-function 'ignore
      column-number-mode t
      echo-keystrokes .1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      require-final-newline t
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 100
      xterm-mouse-mode t
      org-log-done t
      org-src-fontify-natively t
      initial-scratch-message ""
      ;; scroll setting
      scroll-margin 2
      scroll-conservatively 2
      scroll-preserve-screen-position 1
      ;; set mysql client output to vertical instead of table
      sql-mysql-options (list "-E")
      ;; don't pop a new frame for open file
      ns-pop-up-frames nil)


;; turn off interfaces early in startup to avoid momentary display.
;; menu bar is already visible on OX X, so keep it
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
  (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; settings for Windowed systems
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; settings for OS X
(when (eq system-type 'darwin)
  (set-default-font "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :height 110)
  (setq mouse-wheel-scroll-amount '(0.01)))

;; prefer utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; ANSI colors
;; for shells
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
;; for existing files - helper function to colorize
(require 'ansi-color)
(defun lliles-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; don't wrap lines in partial or full windows
(setq truncate-partial-width-windows t)
(set-default 'truncate-lines t)

;; file local variables considered safe
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; transparently open compressed files
(auto-compression-mode t)


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
