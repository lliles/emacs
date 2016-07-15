;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; make sure exec-path and friends are correct
(when (memq window-system '(ns))
  (exec-path-from-shell-initialize))

;; prefer utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; for processing ansi escape sequences as colors
;; for this to have any effect, this must be done
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(ansi-color-for-comint-mode-on)

(setq visible-bell nil
      ring-bell-function 'ignore
      column-number-mode t
      echo-keystrokes .1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      shift-select-mode nil
      mouse-yank-at-point t
      require-final-newline t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      save-place-file (concat user-emacs-directory "places")
      org-log-done t
      org-src-fontify-natively t
      initial-scratch-message ""
      ;; scroll setting
      scroll-margin 2
      scroll-conservatively 2
      scroll-preserve-screen-position 1
      ;; set mysql client output to vertical instead of table
      sql-mysql-options (list "-E")
      ;; don't wrap lines in partial width windows
      truncate-partial-width-windows t
      ;; don't pop a new frame for open file
      ns-pop-up-frames nil)

;; don't wrap lines at all!
(set-default 'truncate-lines t)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Transparently open compressed files
(auto-compression-mode t)

;; Save a list of recent files visited.
(recentf-mode 1)

;; enable paren-face to dim parens
(global-paren-face-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
;(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Hippie expand: at times perhaps too hip
(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat user-emacs-directory "backups")))))
(setq tramp-backup-directory-alist backup-directory-alist)

;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; default to unified diffs
(setq diff-switches "-u -w"
      magit-diff-options "-w")

;; platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  (set-default-font
   "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :height 110)
  (setq mouse-wheel-scroll-amount '(0.01)))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ttl 'toggle-truncate-lines)

;; enable company mode everywhere
(add-hook 'after-init-hook 'global-company-mode)
