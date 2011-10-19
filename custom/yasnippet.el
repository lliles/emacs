(require 'yasnippet)
(yas/initialize)

(setq yas/root-directory (concat user-emacs-directory "snippets"))
(yas/load-directory yas/root-directory)

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

