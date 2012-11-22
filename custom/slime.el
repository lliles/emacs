(setq inferior-lisp-program "/usr/local/bin/clisp") ; your Lisp system
(add-to-list 'load-path "~/.emacs.d/slime")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))
