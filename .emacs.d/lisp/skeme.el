(defvar skeme-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skm\\'" . skeme-mode))

;; (defun skeme-indent-line ()
;;   "Indent current line of Skeme code."
;;   (interactive)
;;   (let ((savep (> (current-column) (current-indentation)))
;;         (indent (condition-case nil (max (skeme-calculate-indentation) 0)
;;                   (error 0))))
;;     (if savep
;;         (save-excursion (indent-line-to indent))
;;       (indent-line-to indent))))

;; (defun skeme-calculate-indentation ()
;;   "Return the column to which the current line should be indented."
;;   (save-excursion
;;     (back-to-indentation)
;;     (backward-up-list)
;;     (+ (current-column) 1)))

(load "scheme.el")  ; This is to reference scheme-indent-function
(define-derived-mode skeme-mode prog-mode "Skeme"
  "Major mode for Skeme (not a real language)"
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'scheme-indent-function)
  )

(provide 'skeme-mode)
