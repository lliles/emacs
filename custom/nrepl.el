(require 'nrepl)

(setq nrepl-history-file "~/.emacs.d/nrepl-history")

;; hides 'nrepl-server' and 'nrepl-connection' buffers from appearing
;; in switch-to-buffer (space will show them)
;;(setq nrepl-hide-special-buffers t)

;; stop the error buffer from popping up at all
;;(setq nrepl-popup-stacktraces nil)

;; enable error buffer popping in the REPL
(setq nrepl-popup-stacktraces-in-repl t)

;; enable paredit 
(add-hook 'nrepl-mode-hook 'paredit-mode) 

;; enable subword mode (i.e. 'EmacsSubwordMode' contains 'Emacs', 'Subword', 'Mode'
(add-hook 'nrepl-mode-hook 'subword-mode)

;; enable eldoc mode
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; auto completion for NREPL
(require 'ac-nrepl)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)

;; enable ritz
(require 'nrepl-ritz)
(define-key nrepl-interaction-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-mode-map (kbd "C-c C-j") 'nrepl-javadoc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-a") 'nrepl-apropos)
(define-key nrepl-mode-map (kbd "C-c C-a") 'nrepl-apropos)

