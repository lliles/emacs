;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

(dolist (r `((?i (file . ,(concat user-emacs-directory "init.el")))))
  (set-register (car r) (cadr r)))
