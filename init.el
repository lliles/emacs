;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;;            -Neal Stephenson, "In the Beginning was the Command Line"

;; setup load path - everything custom in lliles subdirectory
(add-to-list 'load-path (concat user-emacs-directory "lliles/"))
(add-to-list 'load-path (concat user-emacs-directory "lliles/vendor"))

;; TODO reorganize settings to before packages

;; TODO is this still necessary?
;; load themes early to avoid face snatching
(load "custom/themes")

;; setup package
(require 'package)
(setq package-user-dir (concat user-emacs-directory "lliles/packages"))
(setq package-enable-at-startup nil)

;; package sources and priorities
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 15)
        ("gnu"          . 10)
        ("melpa"        . 5)))

;; initialize package
(package-initialize)

;; install use-package and diminish
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

;; require use-package and friends
(require 'use-package)
(require 'diminish)
(require 'bind-key)

;; setup native emacs packages - some settings need to be applied first
;; don't clutter up directories with backup files~
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(use-package browse-url
  :config
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-default-macosx-browser)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300
        recentf-exclude (list "/\\.git/.*\\'"          ; Git contents
                              "/lliles/packages/.*\\'" ; Package files
                              )))

(use-package paren
  :config
  (show-paren-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package windmove
  :config
  (windmove-default-keybindings)) ;; shift+direction

(use-package tramp
  :config
  (setq tramp-backup-directory-alist backup-directory-alist))

(use-package hi-lock
  :diminish hi-lock-mode)

(use-package simple
  :config
  (setq column-number-mode t
        transient-mark-mode t))

(use-package font-lock
  :config
  (setq font-lock-maximum-decoration t))

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing lines-tail tabs)
        whitespace-line-column 100))

(use-package ns-win
  :config
  (setq ns-pop-up-frames nil)) ;; don't pop a new frame for open file

(use-package org
  :config
  (setq org-log-done t
        org-src-fontify-natively t))

(use-package sql
  :config
  (setq sql-mysql-options (list "-E"))) 

(use-package diff-mode
  :mode "COMMIT_EDITMSG$") ;; TODO replace with magit?

(use-package ruby-mode
  :mode ("Rakefile$"
         "Gemfiles$"
         "Capfile$"
         "Vagrantfile$"
         "\\.rb$"
         "\\.rake$"
         "\\.ru$"
         "\\.gemspec$"))

(use-package elisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-watchword-highlighting)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-idle-highlight-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(use-package cc-mode
  :config
  (setq c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "gnu")))) ;; or linux?

;; setup third-party packages
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

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'smartparens-strict-mode)
  (add-hook 'clojure-mode-hook 'subword-mode))

(use-package cider
  :ensure t
  :init
  (setq cider-repl-history-file (concat user-emacs-directory "cider-repl-history"))
  :config
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
  (add-hook 'cider-repl-mode-hook 'subword-mode))

(use-package rainbow-mode :ensure t)

(use-package yasnippet :ensure t)

(use-package css-mode
  :ensure t
  :mode "\\.css$")

(use-package groovy-mode :ensure t)

(use-package paren-face
  :ensure t
  :config
  ;; dim paren faces everywhere
  (global-paren-face-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (setq company-tooltip-align-annotations t)
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
  :diminish ivy-mode
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

;; setup local packages
(use-package extraedit :load-path "lliles/vendor")
(use-package misc-cmds :load-path "lliles/vendor")
(use-package save-frame-geometry :load-path "lliles/vendor")
(use-package randomize-region :load-path "lliles/vendor")

;; TODO review all of these - some probably have changed
;; miscellaneous settings
(setq visible-bell nil
      ring-bell-function 'ignore
      echo-keystrokes .1
      inhibit-startup-message t
      require-final-newline t
      initial-scratch-message ""
      ;; scroll setting
      scroll-margin 2
      scroll-conservatively 2
      scroll-preserve-screen-position 1)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; file local variables considered safe
(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 100))

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ttl 'toggle-truncate-lines)

;; don't wrap lines in partial or full windows
(setq truncate-partial-width-windows t)
(set-default 'truncate-lines t)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; turn off interfaces early in startup to avoid momentary display.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; menu bar is already visible on OS X, so keep it
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

;; enable xterm mouse mode
(xterm-mouse-mode)

;; settings for OS X
(when (eq system-type 'darwin)
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
(defun ll-display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; transparently open compressed files
(auto-compression-mode t)

;; TODO love/hate auto-fill... need to look into alternatives
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load custom code
(load "custom/defuns")

;; global keybindings
;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'imenu)

;; File finding
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Should be able to eval-and-replace anywhere.
;; TODO is this hiding something?
;;(global-set-key (kbd "C-c e") 'eval-and-replace)

;; For debugging Emacs modes
;;(global-set-key (kbd "C-c p") 'message-point)
;;(global-set-key (kbd "C-c q") 'join-line)

;; swap command/options keys in mac os x
(when (eq system-type 'darwin) 
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.
(dolist (r `((?i (file . ,(concat user-emacs-directory "init.el")))))
  (set-register (car r) (cadr r)))

;; load system specific code
(load (concat "custom/" system-name) 'noerror)

;; use this instance as a server
(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(smartparens yasnippet use-package rainbow-mode paren-face paredit magit idle-highlight-mode groovy-mode exec-path-from-shell counsel company cider avy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 120 :width normal)))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
