(require 'package)
(setq package-user-dir (concat user-emacs-directory "lliles/packages"))
(setq package-enable-at-startup nil)

;; package sources
(dolist 
  (source '(("marmalade" . "http://marmalade-repo.org/packages/")
            ("melpa"     . "http://melpa.org/packages/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-install-list)
  (let ((custom (concat user-emacs-directory "lliles/custom/" (symbol-name package))))
    (when (not (package-installed-p package))
      (package-install package))
    (load custom 'noerror)))
