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
  (load "font-latex.el")  ;; Stealing code from latex mode

  (defvar font-skeme-keywords nil
    "Keywords for skeme mode")

  (defun skeme-find-balanced-grouping (&optional count depth limit)
    "Return the position of a balanced grouping in a group.
The function scans forward COUNT parenthetical groupings.
Default is 1.  If COUNT is negative, it searches backwards.  With
optional DEPTH>=1, find that outer level.  If LIMIT is non-nil,
do not search further than this position in the buffer."
    (let ((count (if count
                     (if (= count 0) (error "COUNT has to be <> 0") count)
                   1))
          (depth (if depth
                     (if (< depth 1) (error "DEPTH has to be > 0") depth)
                   1)))
      (save-restriction
        (when limit
          (if (> count 0)
              (narrow-to-region (point-min) limit)
            (narrow-to-region limit (point-max))))
        (condition-case nil
            (scan-lists (point) count depth)
          (error nil)))))

  (defun skeme-find-closing-grouping (&optional depth limit)
    "Return the position of the closing brace in a TeX group.
The function assumes that point is inside the group, i.e. after
an opening brace.  With optional DEPTH>=1, find that outer level.
If LIMIT is non-nil, do not search further down than this
position in the buffer."
    (skeme-find-balanced-grouping 1 depth limit))

  (defun font-skeme-match-script (limit)
    "Match subscript/superscript patterns up to LIMIT."
    (let ((re  (rx
                (any "^" "_")
                (group
                 (or (;; Single-character script
                      not (any "\n" "(" "[" "{" ")" "]" "}"))
                     (;; Grouped script
                      group (syntax open-parenthesis)))))))
      (when (re-search-forward re limit t)
        (when (match-end 2)
          ;; Grouped script
          (let ((beg  (match-beginning 2))
                (end  (skeme-find-closing-grouping
                       ;; Don't match groups spanning more than one line
                       ;; to avoid visually wrong indentation in subsequent lines.
                       nil (line-end-position))))
            (store-match-data (if end
                                  (list (match-beginning 0) end beg end)
                                (list beg beg beg beg)))))
        t)))

  (defun font-skeme--get-script-props (pos script-type)
    "Get script properties (helper for font-skeme-script)"
    (let* ((old-raise (or (plist-get (get-text-property pos 'display) 'raise) 0.0))
           (new-level (1+ (or (get-text-property pos 'script-level) 0)))
           (disp-props (copy-sequence (cl-case script-type
                                        (:super (cdr font-latex-script-display))
                                        (:sub   (car font-latex-script-display)))))
           (new-disp-props (let ((raise (plist-get disp-props 'raise))
                                 (nl new-level))
                             (if raise
                                 (plist-put disp-props 'raise
                                            (+ old-raise
                                               (* raise (expt 0.8 (- nl 1)))))
                               disp-props))))
      `(face ,(cl-case script-type
                (:super 'font-latex-superscript-face)
                (:sub   'font-latex-subscript-face))
             script-level ,new-level
             display ,new-disp-props)))

  (defun font-skeme-script (pos)
    "Return face and display spec for subscript/superscript content."
    (let ((extra-props-flag (boundp 'font-lock-extra-managed-props)))
      (if (eq (char-after pos) ?_)
          (font-skeme--get-script-props pos :sub)
        (font-skeme--get-script-props pos :super))))

  (add-to-list 'font-skeme-keywords
               '(;; Find script by calling function `font-skeme-match-script'
                 ;; The font is calculated by `font-skeme-script' (given the matching position stored inside of the match storage)
                 ;; The `1' indexes sub-expression
                 font-skeme-match-script
                 (1 (font-skeme-script (match-beginning 0)) append))
               t)

  (defun font-skeme-unfontify-region (beg end &rest ignored)
    "Unfontify region from BEG to END.
We need this since the raise level does not get reset automatically"
    (font-lock-default-unfontify-region beg end)
    (remove-text-properties beg end '(script-level))
    (let ((start beg))
      (while (< beg end)
        (let ((next (next-single-property-change beg 'display nil end))
              (prop (get-text-property beg 'display)))
          (if (and (eq (car-safe prop) 'raise)
                   (null (cddr prop)))
              (put-text-property beg next 'display nil))
          (setq beg next))))))

(load "scheme.el")  ; We use `scheme-indent-function'
(define-derived-mode skeme-mode prog-mode "Skeme"
  "Major mode for Skeme"
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'scheme-indent-function)
  (setq font-lock-multiline t)
  (setq font-lock-defaults '(font-skeme-keywords))
  (setq font-lock-unfontify-region-function 'font-skeme-unfontify-region)
  )

(provide 'skeme-mode)
