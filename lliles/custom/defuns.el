;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.

;; setup run-coding-hook
(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(defun turn-on-idle-highlight-mode ()
  (idle-highlight-mode t))

(add-hook 'coding-hook 'local-column-number-mode)
;; this is annoying when temporarily commenting code
;;(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-idle-highlight-mode)

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

;; setup cleanup-buffer
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;; bound in bindings.el
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

;; used in lisp.el
(defun turn-on-paredit ()
  (paredit-mode t))

;; Other
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun insert-date ()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(defun message-point ()
  (interactive)
  (message "%s" (point)))

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

;; custom functions
(defun longest-line-length ()
  "Returns the length of the longest line in the current buffer.
If called interactively, a message is printed in the echo area,
otherwise just the length is returned."
  (interactive)
  (let (longest-line)
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (setq longest-line (current-column))
      (while (< (forward-line 1) 1)
        (end-of-line)
        (if (> (current-column) longest-line)
            (setq longest-line (current-column))))
      (if (called-interactively-p)
          (message "Longest line length: %d" longest-line)))
    longest-line))

(defun mark-buffer-as-rectangle ()
  "Puts mark at beginning of current buffer and moves point
to the last line and column needed to capture all lines in
the buffer as a rectangle. Appends spaces to the last line
as needed to match the length of the longest line."
  (interactive)
  (let ((longest-line (longest-line-length)))
    (goto-char (point-min))
    (push-mark)
    (goto-char (point-max))
    (while (< (current-column) (+ longest-line 1))
      (insert " "))))

(defun kill-rectangle-save ()
  "Save the rectangle as if killed with kill-rectangle, but
don't delete it."
  (message "Implement me!!"))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun unfill-paragraph ()
  "This command does the reverse of `fill-paragraph' by setting
fill-column to a really large number then calling `fill-paragraph'."
  (interactive)
  (let ((fill-column 90000000))
    (fill-paragraph nil)))

(defun unfill-region (start end)
  "This command does the reverse of `fill-region' by setting
fill-column to a really large number then calling `fill-region'."
  (interactive "r")
  (let ((fill-column 90000000))
    (fill-region start end)))

(defun refill-paragraphs-to-be-one-line ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(defun org-to-confluence ()
  "change stars to headings and refill paragraphs"
  (interactive)
  (replace-regexp "\\*\\*\\*" "h4." nil (point-min) (point-max))
  (replace-regexp "\\*\\*" "h3." nil (point-min) (point-max))
  (replace-regexp "\\*" "h2." nil (point-min) (point-max))
  (refill-paragraphs-to-be-one-line))

(defun toggle-window-dedicated ()
  "Toggle whether the current window is dedicated or not"
  (interactive)
  (message
   (let (window (get-buffer-window (current-buffer)))
     (if (window-dedicated-p window)
         (progn (set-window-dedicated-p window nil) "Window '%s' is normal")
       (progn (set-window-dedicated-p window 1) "Window '%s' is dedicated")))
   (current-buffer)))

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
