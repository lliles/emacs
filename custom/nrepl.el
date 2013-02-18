;; enable paredit 
(add-hook 'nrepl-mode-hook 'paredit-mode) 

;; enable eldoc in clojure buffers
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode) 

;; stop the error buffer from popping up 
;;(setq nrepl-popup-stacktraces nil)

;; enable ritz
(add-hook 'nrepl-interaction-mode-hook 'nrepl-mode-setup-require-ritz)
(defun nrepl-mode-setup-require-ritz ()
  (require 'nrepl-ritz))

