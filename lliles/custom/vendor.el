(add-to-list 'load-path (concat user-emacs-directory "lliles/vendor"))

(dolist (library vendor-install-list)
  (let* ((file (symbol-name library))
         (normal (concat user-emacs-directory "lliles/vendor/" file))
         (suffix (concat normal ".el"))
         (custom (concat user-emacs-directory "lliles/custom/" file)))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))
    (when (file-exists-p custom) (load custom))))
