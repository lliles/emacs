(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat dotfiles-dir "vendor/" file))
         (suffix (concat normal ".el"))
         (custom (concat dotfiles-dir "custom/" file ".el")))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (require library)))
    (when (file-exists-p custom) (message (concat "loading " dotfiles-dir)) (load custom))))
