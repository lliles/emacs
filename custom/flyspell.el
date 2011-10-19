;; flyspell setup for os x (install aspell)
(when (eq system-type 'darwin)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-list-command "list")
  (setq ispell-extra-args '("--sug-mode=ultra"))
  (setq flyspell-issue-message-flag nil))
