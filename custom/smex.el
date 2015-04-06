(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(global-set-key (kbd "C-x C-m") 'smex)

;;Like `smex', but limited to commands that are relevant to the active major mode.
;;(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)

