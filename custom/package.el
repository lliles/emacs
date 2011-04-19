(require 'cl)

;; setup ELPA (this will be part of emacs 24)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(defun lliles-install-packages ()
  "Install all packages in lliles-packages that aren't installed."
  (interactive)
  (dolist (package lliles-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

;; check that packages are installed
(unless package-archive-contents (package-refresh-contents))
(lliles-install-packages)

