(require 'package)

;; Version 27.0 automatically initializes packages for you
(when (version< emacs-version "27.0")
  (package-initialize))

(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;; Ignore case in minibuffer's tab completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Load THE theme
(load-theme 'wheatgrass)

(;; Windows redoer
 winner-mode 1)

(;; Don't skip the screen when scrolling up or down
 setq scroll-conservatively 100)

(;; Something really annoying?
 setq ring-bell-function 'ignore)

(;; Show where the cursor is
 global-hl-line-mode)

;;; Getting rid of really annoying buffers
;; Removes *messages* buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

(;; Don't show *Buffer list* when opening multiple files at the same time.
 setq inhibit-startup-buffer-menu t)
(;; Don't show Welcome Screen when opening up
 setq inhibit-startup-screen t)

(;; No more typing the whole yes or no. Just y or n will do.
 fset 'yes-or-no-p 'y-or-n-p)

(;; Adding new file extension to modes
 setq auto-mode-alist (append '(("\\.rkt\\'" . scheme-mode)
                                ("\\.pl$"    . prolog-mode))
                              auto-mode-alist))

;; Get rid of UI elements
(menu-bar-mode     -1)
(toggle-scroll-bar -1)
(tool-bar-mode     -1)

(;; Auto-pair
 electric-pair-mode)

;;; Packages
;; This is why I'm here
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  (;; Do not regexp search when type "/"
   setq evil-regexp-search nil)

  ;; Switch line highlighting off when in insert mode.
  (add-hook 'evil-insert-state-entry-hook
            '(lambda () (global-hl-line-mode -1)))
  (add-hook 'evil-normal-state-entry-hook
            '(lambda () (global-hl-line-mode)))

  ;; Surround
  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode))

  ;; Auto-center search result
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (use-package evil-commentary
    ;; Commentary
    :ensure t
    :config (evil-commentary-mode)))





;;; Key bindings
(progn
 (;; No more M-x! Use smex instead of evil-ex
  evil-define-key 'normal 'global ";" 'smex)
 (evil-define-key 'normal 'global "a" 'evil-append-line)
 (evil-define-key 'normal 'global "A" 'evil-append)
 (evil-define-key 'normal 'global "p" 'evil-paste-before)
 (evil-define-key 'normal 'global "P" 'evil-paste-after)
 (evil-define-key 'normal 'global "W" 'forward-sexp)
 (evil-define-key 'normal 'global "B" 'backward-sexp)
 (evil-define-key 'normal 'global (kbd "<up>") 'evil-scroll-line-up)
 (evil-define-key 'normal 'global (kbd "<down>") 'evil-scroll-line-down)
 (evil-define-key 'normal 'global (kbd "<left>") 'previous-buffer)
 (evil-define-key 'normal 'global (kbd "<right>") 'next-buffer)
 (evil-define-key 'normal 'global (kbd "RET") 'evil-write-all)
 (evil-define-key 'normal 'global (kbd "K") 'open-line)
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
 (evil-define-key 'normal 'global (kbd "C-a") 'mark-whole-buffer)
 (evil-define-key 'normal 'global (kbd "DEL") 'backward-delete-char-untabify)
 (evil-define-key 'normal 'global (kbd "C-.") 'next-buffer)
 (evil-define-key 'normal 'global (kbd "C-,") 'previous-buffer)
 (evil-define-key 'normal 'global (kbd "\\")  (lambda () (interactive) (message "Want Enter?")))

 (evil-define-key 'insert 'global (kbd "C-.") 'next-buffer)
 (evil-define-key 'insert 'global (kbd "C-,") 'previous-buffer)
 (evil-define-key 'insert 'global (kbd "C-v") 'evil-paste-before)

 (evil-define-key 'visual 'global (kbd "TAB") 'indent-rigidly)
 (evil-define-key 'visual 'global (kbd ";")   'smex))

;;; Some vital command alias
(defalias 'k 'kill-buffer-and-window)
(defalias 'f 'ido-find-file)
(defalias 'b 'ido-switch-buffer)
(defalias 'ls 'buffer-menu)






(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(use-package avy
  ;; The dopest snipe package ever
  :ensure t
  :config
  (evil-define-key 'normal 'global (kbd "s") #'avy-goto-char-2))

(;; Why even bother with the default status bar?
 use-package spaceline
 :ensure t
 :init (setq powerline-default-separator 'arrow)
 :config
 (require 'spaceline-config)
 (spaceline-spacemacs-theme))

(use-package ido-vertical-mode
  ;; Ido-mode: a regexp smart search framework
  :ensure t
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching nil
        ido-create-new-buffer 'always
        ido-everywhere t
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always)
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package smex
  ;; Command completion, using Ido above
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package beacon
  ;; Highlight the cursor a bit when switching buffer
  :ensure t
  :init (beacon-mode 1))

(use-package aggressive-indent
  ;; No more worries about lisp indentation
  :ensure t
  :hook ((prog-mode) . aggressive-indent-mode))

(use-package linum-relative
  ;; Relative line number
  :ensure t
  :init
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode))

(;; Show columns
 column-number-mode 1)

;; List of buffers to not open in a new window
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Help*")

(;; Color those parentheses
 use-package rainbow-delimiters
 :ensure t
 :config
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
 (add-hook 'text-mode-hook 'rainbow-delimiters-mode))

(;; Color those identifiers
 use-package rainbow-identifiers
 :ensure t
 :config
 (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
 (add-hook 'text-mode-hook 'rainbow-identifiers-mode))

(;; No tabs!
 setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(;; Automatically update changed buffer
 global-auto-revert-mode 1)

(progn
  ;; Highlight matching brackets
  (show-paren-mode)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package magit
  ;; Git package
  :ensure t)

(use-package expand-region
  ;; Magic that I saw once.
  :ensure t
  :config
  (evil-define-key 'normal 'global "q" #'er/expand-region)
  (evil-define-key 'normal 'global "Q" #'er/contract-region))

(use-package popup-kill-ring
  ;; Use this to yank multiple things.
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package evil-numbers
  ;; Vim Numbering
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C--") 'evil-numbers/dec-at-pt))

(add-hook
 ;; Remove completion buffer when done
 'minibuffer-exit-hook
 '(lambda ()
    (let ((buffer "*Completions*"))
      (and (get-buffer buffer)
         (kill-buffer buffer)))))

(; Save current desktop configs on exit
 desktop-save-mode 1)

(;; Please don't ask whether to kill running processes
 add-hook 'comint-exec-hook
 (lambda () (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer)) nil)))

(setq
 ;; stop making backup files
 make-backup-files nil
 auto-save-default nil)

(use-package math-symbol-lists
  :ensure t
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (mapc
   ;; My rules
   (lambda (x)
     (quail-defrule (car x) (cadr x)))
   '(("\\==" ?≡) ("\\LRa" ?⇔) ("\\<=>" ?⇔) ("\\Ra" ?➾) ("\\=>" ?➾) ("\\ra" ?→) ("\\->" ?→) ("\\-->" ?⟶) ("\\la" ?←) ("\\<-" ?←) ("\\.<" ?⬸) ("\\.>" ?⤑) ("\\lra" ?↔) ("\\<->" ?↔) ("\\up" ?↑) ("\\down" ?↓) ("\\h->" ?↪) ("\\ul" ?↖) ("\\ur" ?↗) ("\\dl" ?↙) ("\\dr" ?↘) ("\\o<" ?⟲) ("\\refl" ?⟲) ("\\o>" ?⟳) ("\\lla" ?↞) ("\\<<-" ?↞) ("\\rra" ?↠) ("\\trans" ?↠) ("\\->>" ?↠) ("\\lr2" ?⇄) ("\\-><" ?⇄) ("\\symm" ?⇄) ("\\==>" ?⟹) ("\\idem" ?⊸) ("\\-o" ?⊸)
     ("\\sub" ?⊆) ("\\sup" ?⊇)
     ("\\ex" ?∃) ("\\for" ?∀)
     ("\\lang" "⟨⟩")
     ("\\+-" ?±) ("\\<=" ?≤) ("\\>=" ?≥) ("\\=~" ?≅)
     ("\\Nat" ?ℕ) ("\\Int" ?ℤ)
     ("\\and" ?∧) ("\\or" ?v) ("\\false" ?⊥) ("\\|=" ?⊨) ("\\|-" ?⊢)
     ("\\cancer" ?♋)))
  (;; math-symbol-list rules
   mapc (lambda (x)
          (if (cddr x)
              (quail-defrule (cadr x) (car (cddr x)))))
   (append
    math-symbol-list-basic
    math-symbol-list-extended
    math-symbol-list-subscripts
    math-symbol-list-superscripts))
  (add-hook 'text-mode-hook
            (lambda () (set-input-method "math")))

  ;; The fonts are: mscr (script), mbfscr (bold script), mfrak (frankfurt), mbf (boldface), Bbb (Double stroke)
  )

(add-hook
 ;; Delete trailing whitespaces on save.
 'before-save-hook 'delete-trailing-whitespace)

;;; Fix lisp indent
(let ((sif 'scheme-indent-function))
  (put 'lam            sif 'defun)
  (put 'def            sif 1)
  (put 'set!           sif 1)
  (put 'class          sif 1)
  (put 'class*         sif 2)
  (put 'match          sif 1)
  (put 'match*         sif 1)
  (put 'send           sif 2)
  (put 'for            sif 1)
  (put 'for*           sif 1)
  (put 'for/list       sif 1)
  (put 'for*/list      sif 1)
  (put 'for/seteq      sif 1)
  (put 'for/set        sif 1)
  (put 'for/or         sif 1)
  (put 'let/cc         sif 1)
  (put 'let/ec         sif 1)
  (put 'trace-let      sif 2)
  (put 'for/and        sif 1)
  (put 'for/andb       sif 1)
  (put 'for/orb        sif 1)
  (put 'for>>          sif 2)
  (put 'for/fold       sif 2)
  (put 'for*/or        sif 1)
  (put 'struct         sif 2)
  (put 'apply          sif 1)
  (put 'for*/and       sif 1)
  (put 'generator      sif 1)
  (put 'with-handlers  sif 1)
  (put 'while          sif 1)
  (put 'place          sif 1)
  (put 'trace-lambda   sif 'defun)
  (put 'trace-define   sif 1)
  (put 'with-syntax    sif 1)
  ;; miniKanren
  (put 'fresh     sif 1)
  (put 'eigen     sif 1)
  (put 'run       sif 2)
  (put 'run*      sif 1)
  (put 'run*au    sif 1)
  (put 'run*su    sif 1)
  (put 'lambdag@  sif 1)
  (put 'letg@     sif 1)
  (put 'lambdaf@  sif 1)
  (put 'case-inf  sif 1)
  (put 'project   sif 1)
  (put 'pmatch    sif 1)
  (put 'match     sif 1)
  (put 'matche    sif 1)
  (put 'fresht    sif 1)
  (put 'letv      sif 1)
  (put 'take      sif 1)
  (put 'case-term sif 1)
  (put 'mv-let    sif 1)
  ;; Misc
  (put 'go-on     sif 1)
  (put 'prove     sif 1)
  )

(defun gk-display-buffer-please-no (buf &rest ignore)
  (error "Inhibited buffer: %s" (buffer-name buf)))
(setq display-buffer-alist
      `('("\\*Quail Completions" . (gk-display-buffer-please-no)) . ,display-buffer-alist))

;;; Custom functions
(defun deGreek ()
  ;; deGreek: at least I know how to Emacs Lisp!. You can start with either, and the other one will finish the job.
  (interactive)
  (let ((egfl #'evil-goto-first-line))
    (replace-string "λ" "lambda") (funcall egfl)
    (replace-string "→" "->") (funcall egfl)
    (replace-string "Γ" "Gamma") (funcall egfl)
    (replace-string "ρ" "rho") (funcall egfl)))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun get-selected-text (start end)
  (interactive "r")
  (buffer-substring start end))

(defun dot ()
  (interactive)
  (if (use-region-p)
      (let ((selected-text (call-interactively 'get-selected-text)))
        (write-region selected-text nil "~/note/graph" nil)
        (shell-command "dot -Tpng ~/note/graph -o ~/note/graph.png")
        (sit-for 1)
        (find-file "~/note/graph.png"))
    (message "Select something first!")))

(defun scot ()
  (interactive)
  (if (use-region-p)
      (let ((selected-text (call-interactively 'get-selected-text)))
        (write-region selected-text nil "~/note/graphin" nil)
        (shell-command "scheme ~/note/scheme-graph.scm")
        (sit-for 1)
        (find-file "~/note/graph.png"))
    (message "Select something first!")))

;; Binary search: zingming.wordpress.com
(defun line-number ()
  (string-to-number (format-mode-line "%l")))
(defun line-count ()
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
  (evil-next-line (/ (- bin-upper bin-lower) 2)))

;; Pro lisp movements
(setq evil-move-beyond-eol t)           ; The magic is here
(progn
  (evil-define-key 'normal 'global (kbd "M-h") 'backward-sexp)
  (evil-define-key 'normal 'global (kbd "M-l") 'forward-sexp)
  (evil-define-key 'normal 'global (kbd "M-k") 'backward-up-list)
  (evil-define-key 'normal 'global (kbd "M-j") 'down-list))






;;; Automatic Settings (DON'T TOUCH BEYOND THIS POINT)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lisp disable-mouse math-symbol-lists rainbow-identifiers spaceline avy smex ido-vertical-mode beacon evil-numbers evil-lion evil-commentary rainbow-delimiters linum-relative evil-surround evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 203 :width normal))))
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
