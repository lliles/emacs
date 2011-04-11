(require 'cl)

;; setup ELPA (this will be part of emacs 24)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(defvar lliles-packages (list 'idle-highlight
                               'ruby-mode
                               'inf-ruby
                               'css-mode
                               'find-file-in-project
                               'magit
                               'gist)
  "packages that should be installed")

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

