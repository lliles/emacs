;; default indentation to 4, but let SGML mode guess, too.
(defun ll-sgml-mode-hook ()
  (set (make-local-variable 'sgml-basic-offset) 4)
  (sgml-guess-indent))

(add-hook 'sgml-mode-hook 'll-sgml-mode-hook)


;; change isearch to stop at beginning of word
(defun ll-goto-match-beginning ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'll-goto-match-beginning)

(defadvice isearch-exit (after ll-goto-match-beginning activate)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))
