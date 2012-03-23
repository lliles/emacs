(require 'package)
(setq package-user-dir (concat user-emacs-directory "packages"))

;; package sources
(dolist 
  (source '(("marmalade" . "http://marmalade-repo.org/packages/")
            ("elpa"      . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-install-list)
  (let ((custom (concat user-emacs-directory "custom/" (symbol-name package))))
    (when (not (package-installed-p package))
      (package-install package))
    (when (file-exists-p custom)
      (load custom))))
