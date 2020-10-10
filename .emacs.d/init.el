;; (server-start)  ;; This is for emacs client, which we're not using
(defun void ()
  (interactive)
  (message "Void function called (you made a typing mistake)"))

(require 'package)

(when (version< emacs-version "27.0")
  (package-initialize))

(setq package-archives
      ;; All archives should be "http://...", but somehow melpa is using tls wtf?
      ;; Anywa, install "gnutls-bin" and it'll be good
      (let ((proto "http://"))
        `(("elpy"         . ,(concat proto "jorgenschaefer.github.io/packages/"))
          ("melpa"        . ,(concat proto "melpa.org/packages/"))
          ("gnu"          . ,(concat proto "elpa.gnu.org/packages/"))
          ("melpa-stable" . ,(concat proto "stable.melpa.org/packages/")))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq completion-ignore-case t ; Ignore case in minibuffer's tab completion
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(winner-mode 1)  ; Windows redoer

(setq scroll-conservatively 100)  ; Don't skip the screen when scrolling up or down

(setq ring-bell-function 'ignore)  ; Something really annoying?

;; (global-hl-line-mode)  ; Show where the cursor is, disabled because messes with line width

(progn  ; Getting rid of really annoying stuffs
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t))

(fset 'yes-or-no-p 'y-or-n-p)  ; No more typing the whole yes or no

(add-to-list 'load-path "~/.emacs.d/lisp/")

(progn  ; Get rid of UI elements
  (menu-bar-mode     -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode     -1))

(;; Auto-pair
 electric-pair-mode)

;;; Packages
;; This is why I'm here
(use-package evil
  :init
  (setq evil-regexp-search nil)

  :config
  (evil-mode 1)

  ;; Switch line highlighting off when in insert mode. (no thanks)
  ;; (add-hook 'evil-insert-state-entry-hook
  ;;           '(lambda () (global-hl-line-mode -1)))
  ;; (add-hook 'evil-normal-state-entry-hook
  ;;           '(lambda () (global-hl-line-mode 1)))

  ;; Auto-center search result
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)))

(use-package evil-commentary
  :config (evil-commentary-mode))

(use-package sudo-edit)

(progn  ;; Extempore
  (unless (fboundp 'eldoc-beginning-of-sexp)  ;; Hacking to get extempore-mode
    (defalias 'eldoc-beginning-of-sexp 'elisp--beginning-of-sexp))

  (use-package extempore-mode
    :config
    (setq extempore-path "~/extempore/")
    (evil-define-key 'normal extempore-mode-map (kbd "X")
      (lambda ()
        (interactive)
        (if (use-region-p)
            (extempore-send-region (region-beginning) (region-end))
          (extempore-send-definition))))))

(progn  ;; Emacs Lisp
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "X")
    (lambda ()
      (interactive)
      (save-excursion
        (let ((beg (point))
              (end (progn (forward-sexp) (point))))
          (eval-region beg end))))))

(use-package disable-mouse
  :config
  ;; (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-insert-state-map
              ;; evil-emacs-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-motion-state-map
              evil-operator-state-map
              evil-outer-text-objects-map
              evil-inner-text-objects-map
              evil-replace-state-map)))

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package evil-lion
  :config
  (evil-lion-mode)
  (setq evil-lion-squeeze-spaces nil)  ;; "t" will break indentation
  (defalias 'tab  ;; Formatting a table, by "space"
    ;; You have to select whole lines, otherwise lion wouldn't know about the space before the first line, and it'll do some goofy things
    ;; Btw, it only works when the matrix is on its own line
    ;; Yeah, I think I'll have to write my own function...
    (lambda (beg end) (interactive "r")
      (evil-lion--align-region
       'left  ;; "right" doesn't work
       nil  ;; "count = nil" means that we align all
       beg end
       (rx (1+ (or alnum "+" "-" "|" "(" ")" "/" "*"))
           (or blank eol "]"))))))

(use-package evil-surround
  :config
  (global-evil-surround-mode)
  (evil-define-key 'visual 'global (kbd "s") #'evil-surround-region))

(use-package avy  ; The dopest snipe package ever
  :config
  (evil-define-key 'normal 'global
    (kbd "f") #'evil-avy-goto-char
    (kbd "s") #'evil-avy-goto-char-2)
  (evil-define-key  ;; Binding to `S' would conflict with `evil-surround' hackery
    'visual 'global (kbd "f") #'evil-avy-goto-char))

(use-package ido-grid-mode  ;; Ido-mode: a regexp smart search framework
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-everywhere t
        ido-use-filename-at-point 'guess)
  :config
  (ido-grid-mode 1)  ;; The normal "ido" layout is insufferable
  (define-key  ;; I wanted it to be "C-w" but somehow it doesn't work
    ido-common-completion-map (kbd "C-e") #'backward-kill-word))

(use-package smex  ; Command completion with Ido
  :init (smex-initialize)
  :config
  (global-set-key (kbd "M-x") #'smex)
  ;; Somehow evil is in motion state when opening help buffers?
  (evil-define-key 'motion Buffer-menu-mode-map ";" #'smex)
  (evil-define-key 'motion help-mode-map ";" #'smex)
  (evil-define-key '(normal visual) 'global ";" #'smex))

(use-package aggressive-indent  ; Resource-inatensive: Use with caution!
  :config
  (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  (add-hook 'text-mode-hook #'aggressive-indent-mode)
  (add-hook 'skeme-mode-hook (lambda () (aggressive-indent-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (aggressive-indent-mode -1)))
  (add-hook 'clojure-mode-hook (lambda () (aggressive-indent-mode -1))))

(column-number-mode 1)  ; Show columns

(use-package rainbow-delimiters  ; Color those parentheses
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers  ; Color those identifiers
  :hook ((prog-mode text-mode) . rainbow-identifiers-mode))

(setq-default indent-tabs-mode nil)  ; No tabs!
(setq-default tab-width 2)

(global-auto-revert-mode 1)  ; Automatically update changed buffer

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(progn  ; Highlight matching brackets
  (show-paren-mode)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package exec-path-from-shell  ;; Something to enale magit?
  :if (memq window-system '(mac ns))
  :config (exec-path-from-shell-initialize))

(use-package magit)

(use-package expand-region
  :config
  (evil-define-key 'normal 'global
    "q" #'er/expand-region
    "Q" #'er/contract-region))

(use-package popup-kill-ring
  :config
  (evil-define-key 'normal 'global "P" #'popup-kill-ring)
  ;; For some reason, evil-define-key doesn't work here
  (define-key popup-kill-ring-keymap (kbd "M-.") 'popup-kill-ring-next)
  (define-key popup-kill-ring-keymap (kbd "M-,") 'popup-kill-ring-previous))

(desktop-save-mode 1)  ; Save current desktop configs on exit

(add-hook 'comint-exec-hook  ; Please don't ask whether to kill running processes
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

(setq  ; stop making backup files
 make-backup-files nil
 auto-save-default nil)

(use-package math-symbol-lists  ; Unicode and (math symbols)/(math notations)
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (mapc  ;; My rules
   (lambda (x)
     (let ((a (car x)) (translation (cadr x)))
       (if (vectorp a)
           (mapc (lambda (key) (quail-defrule key translation)) a)
         (quail-defrule a translation))))
   (append
    '(;; math rules
      ;; Greek letters
      ("\\lam" ?λ) ("\\sig" ?σ) ("\\vphi" ?φ) ("\\eps" ?ϵ)
      ;; Arrows
      (["\\<=>" "\\LRa" "\\Lra" "\\meaning"] ?⇔) (["\\=>" "\\Ra"] ?➾) ("\\La" ?⇐)
      (["\\->" "\\to" "\\then" "\\ra"] ?→) (["\\<-" "\\la"] ?←) ("\\-->" ?⟶) ("\\.<-" ?⬸) ("\\dla" ?⬸) ("\\.->" ?⤑) ("\\dra" ?⤑) ("\\<->" ?↔) ("\\lra" ?↔) ("\\up" ?↑) ("\\ua" ?↑) ("\\da" ?↓) ("\\hra" ?↪) ("\\hla" ?↩) ("\\ul" ?↖) ("\\ur" ?↗) ("\\dl" ?↙) ("\\dr" ?↘) ("\\o<" ?⟲) ("\\refl" ?⟲) ("\\o>" ?⟳) ("\\lla" ?↞) ("\\<<-" ?↞) ("\\rra" ?↠) ("\\trans" ?↠) ("\\->>" ?↠) ("\\lr2" ?⇄) ("\\-><" ?⇄) ("\\symm" ?⇄) ("\\==>" ?⟹) ("\\idem" ?⊸) ("\\-o" ?⊸) ("\\<-|" ?↤) ("\\|->" ?↦)
      ;; Set
      ("\\sub" ?⊆) ("\\sup" ?⊇) ("\\supset" ?⊃) ("\\union" ?∪) ("\\Union" ?⋃) ("\\inter" ?∩) ("\\Inter" ?⋂) ("\\void" ?∅) ("\\power" ?℘) ("\\\\" ?⧵)
      ;; Logic
      ("\\ex" ?∃) ("\\for" ?∀) ("\\and" ?∧) ("\\meet" ?∧) ("\\Meet" ?⋀) ("\\or" ?∨) ("\\join" ?∨) ("\\Join" ?⋁) ("\\false" ?⊥) ("\\|=" ?⊨) ("\\|-" ?⊢)
      ;; Brackets & Pairs
      (["\\<" "\\lang"] ["⟨⟩"]) ("\\ceil" ["⌈⌉"]) ("\\floor" ["⌊⌋"])
      ;; Script Letters
      ("\\nat" ?ℕ) ("\\Nat" ?ℕ) ("\\int" ?ℤ) ("\\Int" ?ℤ) ("\\real" ?ℝ) ("\\Real" ?ℝ) ("\\rat" ?ℚ) ("\\Rat" ?ℚ) ("\\Complex" ?ℂ) ("\\complex" ?ℂ) ("\\com" ?ℂ)
      ;; Others
      ("\\middot" ?ᐧ) ("\\+-" ?±) ("\\by" ?×) ("\\||" ?∥) ("\\<=" ?≤) ("\\>=" ?≥) ("\\=~" ?≅) ("\\iso" ?≅) ("\\~~" ?≈) ("\\deg" ?°) ("\\inv" ?̅) ("\\==" ?≡) ("\\=/" ?≠)
      )))

  (;; math-symbol-list rules
   mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
   (append
    math-symbol-list-basic
    math-symbol-list-extended
    math-symbol-list-subscripts
    math-symbol-list-superscripts))
  ;; The fonts are: mscr (script), mbfscr (bold script), mfrak (frankfurt), mbf (boldface), Bbb (Double stroke)
  )

(progn
  (quail-define-package "fin" "UTF-8" "Fi" t)
  (mapc
   (lambda (x)
     (quail-defrule (car x) (cadr x)))
   '(("Ae"  "Ä")
     (;; This is the way to translate to a string (with multiple chars)
      "Aee" ["Ae"])
     ("ae"  "ä")
     ("aee" ["ae"])
     ("Oe" "Ö")
     ("Oee" ["Oe"])
     ("oe" "ö")
     ("oee" ["oe"]))))

(progn  ;; Buffer Business: display, eliminate, ignore
  (add-hook  ; Remove completion buffer when done
   'minibuffer-exit-hook
   '(lambda ()
      (let ((buffer "*Completions*"))
        (and (get-buffer buffer) (kill-buffer buffer)))))
  (defvar my-skippable-buffers  ;; Regexp to buffers
    ;; Note: not all *XYZ* buffers are bad
    (rx (or "*Messages*"
            "*scratch*"
            "*Quail Completions*"
            "*Buffer List*"
            (seq "magit" (* (any))))))
  (;; Tell ido to ignore the weird buffers
   add-to-list 'ido-ignore-buffers my-skippable-buffers)
  (;; Idk why, but we gotta do this to show ".git" files
   setq ido-ignore-files nil)
  (defun my-change-buffer (change-buffer)
    "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
    (let ((initial (current-buffer)))
      (funcall change-buffer)
      (let ((first-change (current-buffer)))
        (catch 'loop
          (while (string-match my-skippable-buffers (buffer-name))
            (funcall change-buffer)
            (when (eq (current-buffer) first-change)
              (switch-to-buffer initial)
              (throw 'loop t)))))))
  (defun my-next-buffer ()
    "Variant of `next-buffer' that skips `my-skippable-buffers'."
    (interactive)
    (my-change-buffer 'next-buffer))
  (defun my-prev-buffer ()
    "Variant of `previous-buffer' that skips `my-skippable-buffers'."
    (interactive)
    (my-change-buffer 'previous-buffer)))

(let ((sif 'scheme-indent-function))  ; Customize Scheme Indentation
  (put 'defun sif 'defun)
  (mapc (lambda (x)
          (if (listp x)
              (put (car x) sif (cadr x))
            (put x sif 1)))
        '(f@ rec forall ∀ exists ∃ go-on let@ formula@ term@ expr@ lam set! match match* send let/cc let/ec (trace-let 2) struct apply generator with-handlers while place trace-lambda trace-define with-syntax trace-define-syntax (pmatch 2) let-syntax test ; Scheme keywords
             run* run matche project fresh  ; miniKanren
             conjecture prove counter THE assume induction Induction since Since theorem destruct class data type instance subgraph ; Everything else (for my notes)
             )))

;;; Custom functions

(progn  ; Region Search
  (defun search-marked-forward (beg end)
    "Just search the marked region, that's all"
    (interactive "r")
    (evil-normal-state)
    (evil-search (buffer-substring beg end) t))
  (evil-define-key 'visual 'global "*" 'search-marked-forward)

  (defun search-marked-backward (beg end)
    "Just search backward the marked region, that's all"
    (interactive "r")
    (evil-normal-state)
    (evil-search (buffer-substring beg end) nil))
  (evil-define-key 'visual 'global "#" 'search-marked-backward))

(defun ranline ()
  (interactive)
  (goto-line (+ 1 (random (line-count)))))

(setq ffap-url-regexp nil)           ; disable URL features in ffap

(progn  ; Finnish stuff
  (evil-define-key 'visual 'global (kbd "C-f" )
    ;; Translating from finnish to english
    (lambda (beg end)
      (interactive "r")
      (evil-yank beg end)
      (shell-command
       (concat "trans fi:en '"
               (buffer-substring beg end)
               "'"))))
  (add-hook  ; Enter Finnish mode when opening Finnish file
   'find-file-hook
   '(lambda ()
      (if (string-match (buffer-name) "fin.skm")
          (set-input-method 'fin)))))

(add-hook  ; Enter math mode when opening Finnish file
 'find-file-hook
 '(lambda ()
    (if (string-match (buffer-name) "thought.skm")
        (set-input-method 'math))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(use-package company
  :init
  (setq company-show-numbers t)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "M-.") #'company-select-next)
  (define-key company-search-map (kbd "M-.") #'company-select-next)
  (define-key company-active-map (kbd "M-,") #'company-select-previous)
  (define-key company-search-map (kbd "M-,") #'company-select-previous)
  (evil-define-key 'insert 'global (kbd "TAB") #'company-complete))

(progn  ;; Graphviz stuff
  (defun dot (beg end)
    (interactive "r")
    (write-region (buffer-substring beg end) nil "~/notes/data/graph.dot" nil)
    (call-interactively 'view-graph))

  (defun sdot (beg end)
    ;; Instead of writing in "dot", we write in Scheme
    (interactive "r")
    (write-region (buffer-substring beg end) nil "~/notes/data/graph.scm" nil)
    (if (= (shell-command "scheme --script ~/notes/scheme-to-dot.scm") 0)
        (call-interactively 'view-graph)
      (message "That doesn't work!")))

  (defun view-graph ()
    (interactive)
    (;; Compile the dot file to svg
     shell-command "dot -Tsvg ~/notes/data/graph.dot -o ~/notes/data/graph.svg")
    (;; View the svg file
     start-process-shell-command "my-process" nil "open ~/notes/data/graph.svg")))

(progn  ;; Binary search
  (defun line-number ()
    (string-to-number (format-mode-line "%l")))
  (defun line-count ()  ;; count total line of buffer
    (count-lines (point-min) (point-max)))
  (setq bin-lower 0)
  (setq bin-upper (line-count))
  (defun bingo ()
    "Start/reset binary search"
    (interactive)
    (setq bin-lower 0)
    (setq bin-upper (line-count)))
  (defun binup ()
    "Binary search up the file"
    (interactive)
    (setq bin-upper (line-number))
    (evil-previous-line (/ (- bin-upper bin-lower) 2)))
  (defun bindown ()
    "Binary search down the file"
    (interactive)
    (setq bin-lower (line-number))
    (evil-next-line (/ (- bin-upper bin-lower) 2))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer (also copy it).
   From here: stackoverflow.com/q/3669511/4279260"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(defun iv ()
  (interactive)
  (query-replace "iiiii" "v"))

(defun vh ()
  (interactive)
  (query-replace "vvvvvvvvvvvv" "I"))

(defun xv ()
  (interactive)
  (query-replace "x" "vv"))

(progn  ;; Clean buffers that aren't backed by files 
  (defun buffer-backed-by-file-p (buffer)
    (let ((backing-file (buffer-file-name buffer)))
      (if (buffer-modified-p buffer)
          t
        (if backing-file
            (file-exists-p (buffer-file-name buffer))
          t))))

  (defun clean-buffers ()  ;; What the user runs
    (interactive)
    (mapc 'kill-buffer (-remove 'buffer-backed-by-file-p (buffer-list)))))

(progn  ;key bindings
  (global-set-key (kbd "M-,") #'my-prev-buffer)
  (global-set-key (kbd "M-.") #'my-next-buffer)
  (global-set-key (kbd "M-H") #'ns-do-hide-emacs)
  (global-set-key (kbd "s-j") #'evil-window-next)
  (global-set-key (kbd "s-k") #'evil-window-prev)
  (global-set-key (kbd "s-h") #'evil-window-prev)
  (global-set-key (kbd "s-l") #'evil-window-next)
  (global-set-key (kbd "s-0") #'evil-window-delete)
  
  (evil-define-key '(normal visual) 'global
    "I" #'evil-first-non-blank)
  (evil-define-key 'normal 'global
    "A" (lambda () (interactive)
          (evil-end-of-line)
          (evil-backward-char))
    (kbd "<right>") #'my-next-buffer
    (kbd "<left>") #'my-prev-buffer
    (kbd "<up>") #'evil-scroll-line-up
    (kbd "<down>") #'evil-scroll-line-down
    "a" #'evil-append-line
    "p" #'evil-paste-before
    (kbd "RET") #'evil-write-all
    (kbd "K") (lambda () (interactive)
                (save-excursion (call-interactively 'newline)))
    (kbd "SPC") (lambda () (interactive)
                  (insert-char ?\s)
                  (evil-backward-char))
    (kbd "C-j") (lambda () (interactive)
                  (save-excursion (end-of-line)
                                  (open-line 1)))
    (kbd "C-k") (lambda () (interactive)
                  (save-excursion
                    (end-of-line 0)
                    (open-line 1)))
    (kbd "C-a") #'mark-whole-buffer
    (kbd "DEL") #'backward-delete-char-untabify
    (kbd "M-,") #'my-prev-buffer
    (kbd "M-.") #'my-next-buffer
    (kbd "\\")  #'void
    (kbd "TAB") (lambda () (interactive)
                  (save-excursion
                    (evil-indent-line (line-beginning-position) (line-end-position))))
    (kbd "C-M-k") (lambda () (interactive)
                    (call-interactively #'evil-delete-whole-line)
                    (evil-previous-line)
                    (evil-paste-before 1))
    (kbd "C-M-j") (lambda () (interactive)
                    (call-interactively #'evil-delete-whole-line)
                    (evil-next-line)
                    (evil-paste-before 1)))

  (evil-define-key 'visual 'global
    (kbd "TAB") #'indent-region
    "A" (lambda () (interactive) (evil-end-of-line)))

  (evil-define-key 'insert 'global
    (kbd "C-v") #'evil-paste-before
    (kbd "M-k") #'void)
  (when (eq system-type 'darwin) ;; mac specific settings
    (setq mac-function-modifier 'control)
    (setq mac-option-modifier 'super)
    (setq mac-command-modifier 'meta)
    (setq mac-right-command-modifier 'meta)
    (setq mac-right-option-modifier 'control)
    (evil-define-key 'insert 'global (kbd "M-v") #'evil-paste-before)
    (evil-define-key '(insert normal) 'global (kbd "M-q") #'save-buffers-kill-terminal)))

(progn  ; Command alias
  (defalias 'k 'kill-buffer-and-window)
  (defalias '\; #'void)
  (defalias 'f 'ido-find-file)
  (defalias 'b 'ido-switch-buffer)
  (defalias 'ls 'buffer-menu)
  (defalias 'init (lambda () (interactive)
                    (find-file  "~/.emacs.d/init.el")))
  (defalias 'fin (lambda () (interactive)
                   (find-file  "~/notes/fin.skm")))
  (defalias 'thought (lambda () (interactive)
                       (find-file  "~/notes/thought.skm")))
  (defalias 'medals (lambda () (interactive)
                      (find-file  "~/notes/medals.txt"))))

(progn  ;; Pro lisp Movements
  ;; Note: in order for jumps to work, you have to use #' in "evil-define-key"
  (setq evil-move-beyond-eol t)  ; The magic is here
  (evil-define-motion my-backward-up-list ()
    "Go up the list structure"
    :type line
    :jump t
    (backward-up-list 1 t t)  ; By default, it doesn't handle string correctly
    )
  (evil-define-motion my-down-list ()
    "Go down the list structure"
    :type exclusive
    :jump t
    (down-list)  ;; Annoying that it doesn't have the same params as up-list
    )
  (evil-define-motion my-end-of-list ()
    "Go to the end of list"
    :type exclusive
    (up-list 1 t t) (left-char 2))

  (evil-define-motion my-forward-sexp ()
    "inverse of backward-sexp, it will get to the end of the list if we're at the last item (not that I don't like it)"
    :type exclusive
    (condition-case err
        (progn (forward-sexp) (forward-sexp) (backward-sexp))
      (message "%s" (error-message-string err))))

  (evil-define-key 'normal 'global  ;; Map the command key for mac too
    (kbd "M-h") #'backward-sexp
    (kbd "M-l") #'forward-sexp
    (kbd "M-k") #'my-backward-up-list
    (kbd "M-j") #'my-down-list
    (kbd "M-;") #'my-end-of-list
    (kbd "M-t") #'transpose-sexps)

  ;; Modified movement for mhtml mode
  (evil-define-motion my-skip-tag-backward ()
    "Won't move if to the right of one-handed tag, but what'cha gonna do?"
    :type exclusive
    (let ((skip-point
           (save-excursion
             (call-interactively 'sgml-skip-tag-backward)
             (point))))
      (let ((has-end-tag?
             (save-excursion
               (search-backward "</" skip-point t))))
        (if has-end-tag?
            (goto-char skip-point)
          (message "Did not skip over balanced exp")))))
  (evil-define-key 'normal html-mode-map (kbd "M-h") #'my-skip-tag-backward)

  (evil-define-motion my-skip-tag-forward ()
    :type exclusive
    (let ((skip-point
           (save-excursion
             (call-interactively 'sgml-skip-tag-forward)
             (point))))
      (let ((has-start-tag?
             (save-excursion
               (re-search-forward sgml-start-tag-regex skip-point t))))
        (if has-start-tag?
            (goto-char skip-point)
          (message "Did not skip over balanced exp")))))
  (evil-define-key 'normal html-mode-map (kbd "M-l") #'my-skip-tag-forward)

  (evil-define-motion my-down-html ()
    "Hacking"
    :type exclusive
    (let ((after-start-tag
           (save-excursion
             (re-search-forward sgml-start-tag-regex nil t))))
      (let ((has-end-tag?
             (save-excursion
               (search-forward "</" after-start-tag t))))
        (if has-end-tag?
            (message "Encountered an end tag")
          (goto-char after-start-tag)
          (evil-forward-char)))))
  (evil-define-key 'normal html-mode-map (kbd "M-j") #'my-down-html)

  (evil-define-motion my-up-html ()
    "Hacking, using expand-region"
    :jump t
    :type exclusive
    (when (looking-at sgml-start-tag-regex)
      (er/mark-outer-tag))
    (er/mark-outer-tag)
    (deactivate-mark))
  (evil-define-key 'normal html-mode-map (kbd "M-k") #'my-up-html))

(defun csv-to-lines (separator)
  "Converts the current region line, as a csv string,
to a set of independent lines,
splitting the string based on the provided separator.
Still kinda sucks because it can't parse lists"
  (interactive "sEnter separator character: ")  ;; It will become "separator"
  (unless (use-region-p)  ;; Usually I'll keep the cursor on the list
    (er/expand-region 1))
  (let ((text  (call-interactively 'get-selected-text)))
    (call-interactively 'evil-delete)  ;; Delete the selected text
    (;; We don't want to select it
     evil-normal-state)
    (let (_)
      (dolist (element (split-string text separator) _)
        (insert element)
        (call-interactively 'newline)))))

(progn  ;; Language modes & tweaks, language support
  (electric-indent-mode)

  (progn  ;;Skeme, my own mode for note-taking
    (load "skeme"))

  (use-package markdown-mode
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package clojure-mode)

  (setq js-indent-level 2)

  (use-package racket-mode
    :mode (("\\.rkt\\'" . racket-mode)))

  (use-package sql-indent
    :config (add-hook 'sql-mode-hook #'sqlind-minor-mode))

  (define-abbrev-table 'html-mode-abbrev-table
    '(("anchor" "<a target=\"blank\" href=\"\"></a>")
      ("atag" "<a target=\"_blank\" href=\"\"></a>")
      ("csstag" "<link rel=\"stylesheet\" href=\"\">")))

  (defun my-insert-tag (arg)
    (interactive "sTag: ")
    (insert (format "<%s></%s>" arg arg)))
  (evil-define-key '(normal insert) html-mode-map (kbd "C-t") #'my-insert-tag)

  (add-hook 'python-mode-hook
            '(lambda ()
               (aggressive-indent-mode -1)
               (electric-indent-local-mode 'disable)))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (aggressive-indent-mode -1)
               (electric-indent-local-mode 'disable)))

  (use-package dockerfile-mode
    :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

  (use-package terraform-mode
    :config
    ;; #note This matches .tf, .tfstate, .tfstate.backup,...
    (add-to-list 'auto-mode-alist `(,(rx ".tf") . terraform-mode)))
  
  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

  (use-package ansible
    :config
    (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

  (use-package jinja2-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode)))
  
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode)))

(progn  ;; Terminal emulator
  (use-package multi-term
    ;; Note: I can use "rename-uniquely" for now and go with term-mode
    :config
    (setq multi-term-program "/bin/bash"))
  (defadvice ansi-term (after advice-term-line-mode activate)
    (term-line-mode))
  (add-hook 'term-mode-hook  ;; All `term` hooks also get transferred to `multi-term`
            (lambda ()
              (interactive)
              (goto-address-mode 1)  ;; Highlight addresses
              (cd "~/")              ;; At least I wanna open new files from the home directory
              (term-line-mode)       ;; Default to "line mode": Would be cool if it worked...
              ))
  (evil-define-key 'insert term-raw-map  ;; At least we can auto-switch when entering normal state
    (kbd "<escape>") (lambda () (interactive) (term-line-mode) (evil-normal-state)))
  (progn  ;; Pasting in char mode
    (evil-define-key 'insert term-raw-map (kbd "C-v") #'term-paste)
    (if (eq system-type 'darwin)
        (evil-define-key 'insert term-raw-map (kbd "M-v") #'term-paste)))
  (evil-define-key  ;; Press "Enter" to send the whole line
    'normal term-mode-map (kbd "RET") #'term-send-input)
  (let ((term-toggle-mode (lambda ()
                            (interactive)
                            "Toggles term between line mode and char mode"
                            (if (term-in-line-mode)
                                (term-char-mode)
                              (term-line-mode)))))
    ;; #Note: I tried to switch automatically using state entry hook, but no can do
    (evil-define-key '(normal insert) term-mode-map
      (kbd "C-c C-j") term-toggle-mode
      (kbd "C-c C-k") term-toggle-mode))
  (setq term-buffer-maximum-size (lsh 1 14))  ;; 16384 lines
  (eval-after-load 'term
    '(evil-define-key 'insert term-raw-map
       [C-delete] (lambda () (interactive) (term-send-raw-string "\ed"))
       [C-backspace] (lambda () (interactive) (term-send-raw-string "\e\C-h"))
       [M-backspace] (lambda () (interactive) (term-send-raw-string "\e\C-h"))
       [C-left] (lambda () (interactive)
                  (term-send-raw-string "\eb"))
       [C-right] (lambda () (interactive)
                   (term-send-raw-string "\ef"))
       [M-left] (lambda () (interactive)
                  (term-send-raw-string "\eb"))
       [M-right] (lambda () (interactive)
                   (term-send-raw-string "\ef")))))


(progn  ;;Highlighting notes and tags
  (defun khoa-highlight ()
    (let ((regexp-to-highlight
           (rx (or "#" "@")
               (1+ (not (any blank "\"" "\n" "(" ")" ":" "," "\\" "$"))))))
      (font-lock-add-keywords
       nil
       `((,regexp-to-highlight 0 'underline prepend)))))
  
  (add-hook 'skeme-mode-hook #'khoa-highlight)
  (add-hook 'prog-mode-hook  #'khoa-highlight)
  (add-hook 'text-mode-hook  #'khoa-highlight))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes '(wheatgrass))
 '(display-buffer-alist
   '(("\\*shell\\*" display-buffer-same-window)
     ("*Buffer List*" display-buffer-same-window)
     ("*Help*" display-buffer-same-window)))
 '(font-latex-script-display '((raise -0.2) raise 0.2))
 '(ido-ignore-files nil)
 '(package-selected-packages
   '(jinja2-mode jinja2 ansible multi-term sudo-edit yaml-mode exec-path-from-shell terraform-mode dockerfile-mode racket-mode cider clojure-mode text-translator paredit xr texfrag lisp disable-mouse math-symbol-lists rainbow-identifiers spaceline avy smex ido-vertical-mode evil-numbers evil-lion evil-commentary rainbow-delimiters evil-surround evil use-package))
 '(sgml-xml-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "wheat" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 230 :width normal :foundry "nil"))))
 '(font-latex-subscript-face ((t (:height 0.7))))
 '(hl-line ((t (:box (:line-width 2 :color "yellow green" :style released-button)))))
 '(linum ((t (:inherit (shadow default) :height 100))))
 '(match ((t (:background "RoyalBlue3" :underline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark green"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "purple"))))
 '(rainbow-delimiters-unmatched-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark green"))))
 '(show-paren-match ((t (:underline "cyan" :weight extra-bold)))))

(set-face-attribute 'region nil  ;; This has to be here, otw will be overrided by wheatgrass
                    :background "#333"
                    :foreground 'unspecified)
