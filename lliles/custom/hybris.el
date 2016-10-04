;; hybris specific functions
(defun ll-impexinate-region (start end)
  "Replaces beginning of line and all tab characters with semicolons in the region"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (replace-match ";"))
    (goto-char (point-min))
    (while (re-search-forward "	" nil t)
      (replace-match ";"))))

(defun ll-expand-impex (start end)
  "Aligns the impex along semicolons"
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\);" 1 1 t))

(defun ll-collapse-impex (start end)
  "Collapse the impex - opposite of expand-impex"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "\\s-*;" nil t)
      (replace-match ";"))))
