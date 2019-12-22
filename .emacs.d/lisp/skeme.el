(defvar skeme-mode-hook nil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.skm\\'" . skeme-mode))

(defvar skeme-mode-syntax-table nil "Syntax table for `skeme-mode'.")
(setq skeme-mode-syntax-table
      (let ((synTable  (make-syntax-table)))
        ;; lisp style comment: “;; …” (single `;' is not included since it's common)
        (modify-syntax-entry ?\; ". 12b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)
        synTable))

(progn  ;; subscript/superscript highlighting
  (load "font-latex.el")
  (load "latex.el")

  (defvar font-skeme-keywords nil
    "Keywords for skeme mode")

  (defun font-skeme-match-script (limit)
    "Match subscript and superscript patterns up to LIMIT."
    (when (re-search-forward "[_^] *\\([^\n\\{}]\\|\
\\\\\\([a-zA-Z@]+\\|[^ \t\n]\\)\\|\\({\\)\\)" limit t)
      (when (match-end 3)
        (let ((beg  (match-beginning 3))
              (end  (TeX-find-closing-brace
                     ;; Don't match groups spanning more than one line
                     ;; to avoid visually wrong indentation in subsequent lines.
                     nil (line-end-position))))
          (store-match-data (if end
                                (list (match-beginning 0) end beg end)
                              (list beg beg beg beg)))))
      t))

  (defun font-skeme--get-script-props (pos script-type)
    (let* ((old-raise (or (plist-get (get-text-property pos 'display) 'raise) 0.0))
           (new-level (1+ (or (get-text-property pos 'script-level) 0)))
           (disp-props (copy-sequence (cl-case script-type
                                        (:super (cdr font-latex-script-display))
                                        (:sub   (car font-latex-script-display)))))
           (new-disp-props (let ((raise (plist-get disp-props 'raise))
                                 (nl new-level))
                             (if raise
                                 ;; This polynom approximates that the factor
                                 ;; which is multiplied with raise is 1 for nl=1,
                                 ;; 0.8 for nl=2, 0.64 for nl=3, etc. (so always
                                 ;; about 80% of the previous value).
                                 (plist-put disp-props 'raise
                                            (+ old-raise
                                               (* raise
                                                  (+ 1.1965254857142873
                                                     (* nl -0.21841226666666758)
                                                     (* nl nl 0.012018514285714385)))))
                               disp-props))))
      `(face ,(cl-case script-type
                (:super 'font-latex-superscript-face)
                (:sub   'font-latex-subscript-face))
             script-level ,new-level
             display ,new-disp-props)))

  (defun font-skeme-script (pos)
    "Return face and display spec for subscript and superscript content."
    (let ((extra-props-flag (boundp 'font-lock-extra-managed-props)))
      (if (eq (char-after pos) ?_)
          (if extra-props-flag
              (font-skeme--get-script-props pos :sub)
            'font-latex-subscript-face)
        (if extra-props-flag
            (font-skeme--get-script-props pos :super)
          'font-latex-superscript-face))))

  (add-to-list 'font-skeme-keywords
               '(;; Find script by calling function `font-skeme-match-script'
                 ;; The font is calculated by `font-skeme-script'
                 ;; 1 stands indexes sub-expression
                 font-skeme-match-script
                 (1 (font-skeme-script (match-beginning 0)) append))
               t)
  )

(load "scheme.el")  ; We use `scheme-indent-function'
(define-derived-mode skeme-mode prog-mode "Skeme"
  "Major mode for Skeme"
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'scheme-indent-function)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(font-skeme-keywords))
  )

(provide 'skeme-mode)
