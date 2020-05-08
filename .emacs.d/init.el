(require 'package)

(when (version< emacs-version "27.0")
  (package-initialize))

(setq package-archives
      ;; All archives should be "http://..."
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

(setq completion-ignore-case t ; Ignore case in minibuffer's tab completion
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(winner-mode 1)  ; Windows redoer

(setq scroll-conservatively 100)  ; Don't skip the screen when scrolling up or down

(setq ring-bell-function 'ignore)  ; Something really annoying?

(global-hl-line-mode)  ; Show where the cursor is

(progn  ; Getting rid of really annoying stuffs
  (setq-default message-log-max nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t))

(fset 'yes-or-no-p 'y-or-n-p)  ; No more typing the whole yes or no

(setq auto-mode-alist  ; Bind file extension to modes
      (append '(("\\.rkt\\'" . scheme-mode)
                ("\\.pl$"    . prolog-mode))
              auto-mode-alist))
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
  :ensure t
  :init
  (evil-mode 1)

  :config
  (progn
    ;; evil-emacs-state is annoying, the following function and hook automatically
    ;; switch back to evil-normal-state whenever the evil-emacs-state is entered.
    ;; It allows a more consistent navigation experience among all mode maps.
    ;; To enter special commands of custom mode maps, just enter the insert mode :-)
    (defun emacs-state->normal-state ()
      (evil-normal-state)
      (remove-hook 'post-command-hook 'emacs-state->normal-state))
    (add-hook 'evil-emacs-state-entry-hook
              (lambda ()
                (add-hook 'post-command-hook 'emacs-state->normal-state))))

  (;; Do not regexp search when type "/"
   setq evil-regexp-search nil)

  ;; Switch line highlighting off when in insert mode.
  (add-hook 'evil-insert-state-entry-hook
            '(lambda () (global-hl-line-mode -1)))
  (add-hook 'evil-normal-state-entry-hook
            '(lambda () (global-hl-line-mode 1)))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode)
    (evil-define-key 'visual 'global (kbd "s") #'evil-surround-region))

  ;; Auto-center search result
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    ;; (evil-scroll-line-to-center (line-number-at-pos))
    )
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    ;; (evil-scroll-line-to-center (line-number-at-pos))
    )

  (use-package evil-commentary
    :ensure t
    :config (evil-commentary-mode)))

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode)
  (mapc #'disable-mouse-in-keymap
        (list evil-motion-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-insert-state-map)))

(defun disable-all-themes ()
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package fold-this  ; Just hide the region!
  ;; Unfold with "enter"
  :ensure t
  :config
  (fold-this-mode 1)
  (evil-define-key 'normal 'global (kbd "H") #'fold-this-sexp)
  (evil-define-key 'visual 'global (kbd "H") #'fold-this)
  (defun fold-this-sexp ()
    "Fold sexp around point.

  If the point is at a symbol, fold the parent sexp.  If the point
  is in front of a sexp, fold the following sexp."
    (interactive)
    (let* ((region
            (cond
             ((symbol-at-point)
              (save-excursion
                (when (nth 3 (syntax-ppss))
                  (goto-char (nth 8 (syntax-ppss))))
                (backward-up-list)
                (cons (point)
                      (progn
                        (forward-sexp)
                        (point)))))
             ((or (looking-at-p (rx (* blank) "("))
                  (looking-at-p (rx (* blank) "["))
                  (looking-at-p (rx (* blank) "{")))
              (save-excursion
                (skip-syntax-forward " ")
                (cons (point)
                      (progn
                        (forward-sexp)
                        (point)))))
             (t nil)))
           (header (when region
                     (save-excursion
                       (goto-char (car region))
                       (buffer-substring (point) (line-end-position))))))
      (when region
        (fold-this (car region) (cdr region) header)))))

(use-package evil-lion
  :ensure t
  :config
  (defalias 'tab (lambda () (interactive)
                   (save-excursion
                     (unless (use-region-p)
                       (er/expand-region 3))
                     (let ((beg  (region-beginning))
                           (end  (region-end)))
                       (evil-lion-left 0 beg end ?|)
                       (indent-region beg end))))))

(use-package avy  ; The dopest snipe package ever
  :ensure t
  :config
  ;; (evil-define-key 'normal 'global (kbd "f") #'avy-goto-char)
  (evil-define-key 'normal 'global (kbd "s") #'avy-goto-char-2))

(use-package ido-vertical-mode ; Ido-mode: a regexp smart search framework
  :ensure t
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching nil
        ido-create-new-buffer 'always
        ido-everywhere t
        ido-use-filename-at-point 'guess)
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package smex  ; Command completion, using Ido above
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package beacon
  ;; Highlight the cursor a bit when switching buffer
  :ensure t
  :init (beacon-mode 1))

(use-package aggressive-indent  ; Resource-inatensive: Use with caution!
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  (add-hook 'text-mode-hook #'aggressive-indent-mode)
  (add-hook 'skeme-mode-hook (lambda () (aggressive-indent-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (aggressive-indent-mode -1))))

(column-number-mode 1)  ; Show columns

(use-package rainbow-delimiters  ; Color those parentheses
  :ensure t
  :hook ((prog-mode text-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers  ; Color those identifiers
  :ensure t
  :hook ((prog-mode text-mode) . rainbow-identifiers-mode))

(setq-default indent-tabs-mode nil)  ; No tabs!
(setq-default tab-width 2)

(global-auto-revert-mode 1)  ; Automatically update changed buffer

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(progn  ; Highlight matching brackets
  (show-paren-mode)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package magit ; Git package
  :ensure t)

(use-package expand-region ; Magic that I saw once.
  :ensure t
  :config
  (evil-define-key 'normal 'global "q" #'er/expand-region)
  (evil-define-key 'normal 'global "Q" #'er/contract-region))

(use-package popup-kill-ring ; Use this to yank multiple things.
  :ensure t
  :config
  (evil-define-key 'normal 'global "P" #'popup-kill-ring))

(desktop-save-mode 1)  ; Save current desktop configs on exit

(add-hook 'comint-exec-hook  ; Please don't ask whether to kill running processes
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

(setq  ; stop making backup files
 make-backup-files nil
 auto-save-default nil)

(use-package math-symbol-lists  ; Unicode and (math symbols)/(math notations)
  :ensure t
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (mapc  ;; My rules
   (lambda (x)
     (quail-defrule (car x) (cadr x)))
   (append
    '(;; math rules
      ("\\lam" ?λ) ("\\sig" ?σ) ("\\vphi" ?φ)
      ("\\==" ?≡) ("\\=/" ?≠)
      ("\\<=>" ?⇔) ("\\LRa" ?⇔) ("\\Lra" ?⇔) ("\\=>" ?➾) ("\\Ra" ?➾) ("\\La" ?⇐) ("\\->" ?→) ("\\to" ?→) ("\\then" ?→) ("\\ra" ?→) ("\\-->" ?⟶) ("\\<-" ?←) ("\\la" ?←) ("\\.<-" ?⬸) ("\\dla" ?⬸) ("\\.->" ?⤑) ("\\dra" ?⤑) ("\\<->" ?↔) ("\\lra" ?↔) ("\\up" ?↑) ("\\ua" ?↑) ("\\da" ?↓) ("\\hra" ?↪) ("\\hla" ?↩) ("\\ul" ?↖) ("\\ur" ?↗) ("\\dl" ?↙) ("\\dr" ?↘) ("\\o<" ?⟲) ("\\refl" ?⟲) ("\\o>" ?⟳) ("\\lla" ?↞) ("\\<<-" ?↞) ("\\rra" ?↠) ("\\trans" ?↠) ("\\->>" ?↠) ("\\lr2" ?⇄) ("\\-><" ?⇄) ("\\symm" ?⇄) ("\\==>" ?⟹) ("\\idem" ?⊸) ("\\-o" ?⊸) ("\\<-|" ?↤) ("\\|->" ?↦)
      ("\\sub" ?⊆) ("\\sup" ?⊇) ("\\supset" ?⊃) ("\\union" ?∪) ("\\Union" ?⋃) ("\\inter" ?∩) ("\\Inter" ?⋂) ("\\void" ?∅) ("\\power" ?℘)
      ("\\ex" ?∃) ("\\for" ?∀)
      ("\\<" "⟨⟩") ("\\lang" "⟨⟩")
      ("\\+-" ?±) ("\\<=" ?≤) ("\\>=" ?≥) ("\\=~" ?≅) ("\\iso" ?≅) ("\\~~" ?≈)
      ("\\nat" ?ℕ) ("\\Nat" ?ℕ) ("\\int" ?ℤ) ("\\Int" ?ℤ) ("\\real" ?ℝ) ("\\Real" ?ℝ) ("\\rat" ?ℚ) ("\\Rat" ?ℚ)
      ("\\and" ?∧) ("\\meet" ?∧) ("\\Meet" ?⋀) ("\\or" ?∨) ("\\join" ?∨) ("\\Join" ?⋁) ("\\false" ?⊥) ("\\|=" ?⊨) ("\\|-" ?⊢)
      ("\\cancer" ?♋)
      ("\\middot" ?ᐧ))))

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
  (add-hook 'prog-mode-hook (lambda () (set-input-method 'math)))
  (add-hook 'text-mode-hook (lambda () (set-input-method 'math))))

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

  (defvar my-skippable-buffers  ;; Regexp to skip *XYZ* buffers
    (rx (or "*Messages*" "*scratch*" "*Quail Completions*")))

  (add-to-list 'ido-ignore-buffers  ;; Tell ido to ignore the weird asterisk buffers
               my-skippable-buffers)

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

  (defun my-previous-buffer ()
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

(load "skeme")  ; My note-taking mode (it inherits keywords from scheme)

;;; Custom functions

(progn  ; Region Search
  (defun region-search-forward ()
    (interactive)
    (let ((selected (call-interactively 'get-selected-text)))
      (evil-normal-state)
      (evil-search selected t)))
  (evil-define-key 'visual 'global "*" 'region-search-forward)

  (defun region-search-backward ()
    (interactive)
    (let ((selected (call-interactively 'get-selected-text)))
      (evil-normal-state)
      (evil-search selected nil)))
  (evil-define-key 'visual 'global "#" 'region-search-backward))

(defun ranline ()
  (interactive)
  (goto-line (+ 1 (random (line-count)))))

(defun deGreek ()
  ;; deGreek: at least I know how to Emacs Lisp!. You can start with either, and the other one will finish the job.
  (interactive)
  (let ((egfl #'evil-goto-first-line))
    (replace-string "λ" "lambda") (funcall egfl)
    (replace-string "→" "->") (funcall egfl)
    (replace-string "Γ" "Gamma") (funcall egfl)
    (replace-string "ρ" "rho") (funcall egfl)))

(evil-define-key 'visual 'global (kbd "C-f" )
  ;; Translating from finnish to english
  (lambda (beg end)
    (interactive "r")
    (evil-yank beg end)
    (shell-command
     (concat "trans fi:en '"
             (buffer-substring beg end)
             "'"))))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun get-selected-text (start end)
  (interactive "r")
  (buffer-substring start end))

(progn  ;; p5.js
  (defun p5 ()
    ;; Guess a file name, then copy that file to "sketch.js"
    ;; Then open the webpage that renders "sketch.js"
    (interactive)
    (let ((file-name  (ffap-guess-file-name-at-point)))
      (shell-command (format "cp %s ~/note/p5/sketch.js" file-name))
      (call-interactively 'view-p5)))

  (defun view-p5 ()
    (interactive)
    (;; View the html file in a new browser window (not a new tab)
     start-process-shell-command "my-process" nil "chromium-browser --app --new-window ~/note/p5/index.html")))

(progn  ;; Graph
  (defun dot ()
    (interactive)
    (if (use-region-p)
        (let ((selected-text  (call-interactively 'get-selected-text)))
          (write-region selected-text nil "~/note/data/graph.dot" nil)
          (call-interactively 'view-graph))
      (message "Select something first!")))

  (defun sdot ()
    (interactive)
    (if (use-region-p)
        (let ((selected-text  (call-interactively 'get-selected-text)))
          (write-region selected-text nil "~/note/data/graph.scm" nil)
          (if (= (shell-command "scheme --script ~/note/scheme-to-dot.scm") 0)
              (call-interactively 'view-graph)
            (message "That doesn't work!")))
      (message "Select something first!")))

  (defun view-graph ()
    (interactive)
    (;; Compile the dot file to svg
     shell-command "dot -Tsvg ~/note/data/graph.dot -o ~/note/data/graph.svg")
    (;; View the svg file
     start-process-shell-command "my-process" nil "xviewer ~/note/data/graph.svg")))

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

(progn  ; Key bindings
  (;; No more M-x! Use smex instead of evil-ex
   evil-define-key 'normal 'global ";" #'smex)
  (evil-define-key 'normal 'global "I" #'evil-first-non-blank)
  (evil-define-key 'normal 'global "a" #'evil-append-line)
  (evil-define-key 'normal 'global "A" #'evil-append)
  (evil-define-key 'normal 'global "p" #'evil-paste-before)
  (evil-define-key 'normal 'global "P" #'evil-paste-after)
  (evil-define-key 'normal 'global "W" #'forward-sexp)
  (evil-define-key 'normal 'global "B" #'backward-sexp)
  (evil-define-key 'normal 'global (kbd "<up>") #'evil-scroll-line-up)
  (evil-define-key 'normal 'global (kbd "<down>") #'evil-scroll-line-down)
  (evil-define-key 'normal 'global (kbd "<left>") #'my-previous-buffer)
  (evil-define-key 'normal 'global (kbd "<right>") #'my-next-buffer)
  (evil-define-key 'normal 'global (kbd "RET") #'evil-write-all)
  (evil-define-key 'normal 'global (kbd "K")
    (lambda () (interactive)
      (save-excursion (call-interactively 'newline))))
  (evil-define-key 'normal 'global (kbd "SPC") (lambda () (interactive)
                                                 (insert-char ?\s)
                                                 (evil-backward-char)))
  (evil-define-key 'normal 'global (kbd "C-j") (lambda () (interactive)
                                                 (save-excursion
                                                   (end-of-line)
                                                   (open-line 1))))
  (evil-define-key 'normal 'global (kbd "C-k") (lambda () (interactive)
                                                 (save-excursion
                                                   (end-of-line 0)
                                                   (open-line 1))))
  (evil-define-key 'normal 'global (kbd "C-a") #'mark-whole-buffer)
  (evil-define-key 'normal 'global (kbd "DEL") #'backward-delete-char-untabify)
  (evil-define-key 'normal 'global (kbd "C-.") #'my-next-buffer)
  (evil-define-key 'normal 'global (kbd "C-,") #'my-previous-buffer)
  (evil-define-key 'normal 'global (kbd "\\")  (lambda () (interactive) (message "Want Enter?")))
  (evil-define-key 'normal 'global (kbd "TAB") (lambda () (interactive)
                                                 (save-excursion
                                                   (evil-indent-line (line-beginning-position) (line-end-position)))))

  (evil-define-key 'insert 'global (kbd "C-.") #'my-next-buffer)
  (evil-define-key 'insert 'global (kbd "C-,") #'my-previous-buffer)
  (evil-define-key 'insert 'global (kbd "C-v") #'evil-paste-before)

  (evil-define-key 'visual 'global (kbd "TAB") #'indent-region)
  (evil-define-key 'visual 'global ";" #'smex))

(progn  ; Command alias
  (defalias 'k 'kill-buffer-and-window)
  (defalias 'f 'ido-find-file)
  (defalias 'b 'ido-switch-buffer)
  (defalias 'ls 'buffer-menu)
  (defalias 'init (lambda () (interactive)
                    (find-file  "~/.emacs.d/init.el")))
  (defalias 'thought (lambda () (interactive)
                       (find-file  "~/note/thought.skm")))
  (defalias 'lib (lambda () (interactive)
                   (find-file  "~/note/lib.ss")))
  (defalias 'work (lambda () (interactive)
                    (find-file  "~/note/work.md"))))

(progn  ;; Pro lisp Movements
  ;; Note: in order for jumps to work, you have to use #' in "evil-define-key"
  (setq evil-move-beyond-eol t)  ; The magic is here
  (evil-define-motion evil-backward-up-list ()
  "Go up the list structure"
  :type line
  :jump t
  (backward-up-list 1 t t)  ; By default, it doesn't handle string correctly
  )
  (evil-define-motion evil-down-list ()
  "Go down the list structure"
  :type exclusive
  :jump t
  (down-list))
  (evil-define-motion evil-end-of-list ()
  "Go to the end of list"
  :type exclusive
  :jump t
  (up-list 1 t t) (left-char 2))
  (evil-define-key 'normal 'global (kbd "M-h") #'backward-sexp)
  (evil-define-key 'normal 'global (kbd "M-l") #'forward-sexp)
  (evil-define-key 'normal 'global (kbd "M-l") #'forward-sexp)
  (evil-define-key 'normal 'global (kbd "M-;") #'evil-end-of-list)
  (evil-define-key 'normal 'global (kbd "M-k") #'evil-backward-up-list)
  (evil-define-key 'normal 'global (kbd "M-j") #'evil-down-list)
  (evil-define-key 'normal 'global (kbd "M-t") #'transpose-sexps))

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

(progn  ;; Language support
  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package clojure-mode
    :ensure t))

;;; Automatic Settings (DON'T TOUCH BEYOND THIS POINT)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(display-buffer-alist
   (quote
    (("\\*shell\\*" display-buffer-same-window)
     ("*Buffer List*" display-buffer-same-window)
     ("*Help*" display-buffer-same-window))))
 '(font-latex-script-display (quote ((raise -0.2) raise 0.2)))
 '(package-selected-packages
   (quote
    (cider clojure-mode text-translator paredit xr texfrag fold-this lisp disable-mouse math-symbol-lists rainbow-identifiers spaceline avy smex ido-vertical-mode beacon evil-numbers evil-lion evil-commentary rainbow-delimiters evil-surround evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 203 :width normal))))
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
 '(region ((t (:background "#666"))))
 '(show-paren-match ((t (:underline "cyan" :weight extra-bold)))))
