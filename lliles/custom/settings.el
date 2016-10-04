
;; ido-mode is like magic pixie dust!
;;(ido-mode t)
;;(ido-ubiquitous-mode t)
;;(setq ido-enable-prefix nil
;;      ido-enable-flex-matching t
;;      ido-auto-merge-work-directories-length nil
;;      ido-create-new-buffer 'always
;;      ido-use-filename-at-point 'guess
;;      ido-use-virtual-buffers t
;;      ido-handle-duplicate-virtual-buffers 2
;;      ido-max-prospects 10)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Hippie expand: at times perhaps too hip
;;(dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
;;  (delete f hippie-expand-try-functions-list))

;; Add this back in at the end of the list.
;;(add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t)

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat user-emacs-directory "backups")))))
(setq tramp-backup-directory-alist backup-directory-alist)

;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; grep mode ignores
(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; default to unified diffs
(setq diff-switches "-u -w")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rr 'replace-regexp)
(defalias 'qrr 'query-replace-regexp)
(defalias 'ttl 'toggle-truncate-lines)

