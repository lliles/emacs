;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

(dolist (r `((?i (file . ,(concat dotfiles-dir "init.el")))
             (?b (file . ,(concat dotfiles-dir "lliles/bindings.el")))
             (?r (file . ,(concat dotfiles-dir "lliles/registers.el")))))
  (set-register (car r) (cadr r)))
