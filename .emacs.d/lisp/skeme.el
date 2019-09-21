(defvar skeme-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skm\\'" . skeme-mode))

(defvar skeme-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?- "w" st)
    st))

(defun skeme-indent-line ()
  "Indent current line of Skeme code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (skeme-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun skeme-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (back-to-indentation)
    (backward-up-list)
    (+ (current-column) 1)))

(define-derived-mode skeme-mode prog-mode "Skeme"
  "Major mode for Skeme (not a real language)"
  (set (make-local-variable 'indent-line-function) 'skeme-indent-line)
  :syntax-table skeme-mode-syntax-table)

(provide 'skeme-mode)
