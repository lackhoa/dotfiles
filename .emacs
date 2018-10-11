(require 'package)
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

;; Version 27.0 automatically initializes packages for you
(when (version< emacs-version "27.0")
  (package-initialize))

;; Ignore case in minibuffer's tab completion
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; Load THE theme
(load-theme 'wheatgrass)

;; Don't skip the screen when scrolling up or down
(setq scroll-conservatively 100)

;; Something really annoying?
(setq ring-bell-function 'ignore)

;; Show where the cursor is
(global-hl-line-mode)

;;; Getting rid of really annoying buffers
;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")
;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)
;; Don't show Welcome Screen when opening up
(setq inhibit-startup-screen t)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; Adding new file extension to modes
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . prog-mode))

;; Get rid of the UI elements
(menu-bar-mode     -1)
(toggle-scroll-bar -1)
(tool-bar-mode     -1)

;; Auto-pair
(electric-pair-mode)

;;; Packages
;; This is why I'm here
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  ;; Switch line highlighting off when in insert mode.
  (add-hook 'evil-insert-state-entry-hook
            '(lambda () (global-hl-line-mode -1)))
  (add-hook 'evil-normal-state-entry-hook
            '(lambda () (global-hl-line-mode)))

  ;; Surround
  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  ;; Auto-completion
  (use-package company
    :ensure t
    :config
    (global-company-mode)
    (setq company-idle-delay 0.2
          company-selection-wrap-around t)
    (define-key company-active-map [tab] 'company-complete)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    ;; Delete word when in automcomplete
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))

  ;; Auto-center search result
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  ;; Commentary
  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

  ;; The dopest snipe package ever
  (use-package avy
    :ensure t
    :config
    (evil-define-key 'normal 'global (kbd "f") #'avy-goto-char)
    (evil-define-key 'normal 'global (kbd "s") #'avy-goto-char-2))

;;; Key bindings
  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-normal-state-map "a" 'evil-append-line)
  (define-key evil-normal-state-map "A" 'evil-append)
  (define-key evil-normal-state-map "p" 'evil-paste-before)
  (evil-define-key 'insert 'global (kbd "C-v") 'evil-paste-before)
  (define-key evil-normal-state-map "P" 'evil-paste-after)
  (define-key evil-motion-state-map (kbd "RET") 'evil-write-all)
  (evil-define-key 'normal 'global [down] 'evil-scroll-line-down)
  (evil-define-key 'insert 'global [down] 'evil-scroll-line-down)
  (evil-define-key 'normal 'global [up] 'evil-scroll-line-up)
  (evil-define-key 'insert 'global [up] 'evil-scroll-line-up)
  (evil-define-key 'normal 'global [right] 'next-buffer)
  (evil-define-key 'normal 'global [left] 'previous-buffer)
  (evil-define-key 'normal 'global (kbd "K") #'open-line)

  (evil-define-key 'normal 'global (kbd "C-j") #'add-line-below)
  (defun add-line-below ()
    (interactive)
    (save-excursion
      (end-of-line)
      (open-line 1)))

  (evil-define-key 'normal 'global (kbd "C-k") #'add-line-above)
  (defun add-line-above ()
    (interactive)
    (save-excursion
      (end-of-line 0)
      (open-line 1)))

  (evil-define-key 'visual 'global (kbd "TAB") #'indent-rigidly)

;;; A bunch of commands
  (evil-ex-define-cmd "x" 'kill-this-buffer)
  (evil-ex-define-cmd "f" 'find-file)
  (evil-ex-define-cmd "b" 'switch-to-buffer)
  (evil-ex-define-cmd "s" #'replace-regexp-entire-buffer)
  (defun replace-regexp-entire-buffer (pattern replacement)
    "Perform regular-expression replacement throughout buffer."
    (interactive
     (let ((args (query-replace-read-args "Replace" t)))
       (setcdr (cdr args) nil)    ; remove third value returned from query---args
       args))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
        (replace-match replacement)))))

;; Why even bother with the default status bar?
(use-package spaceline
  :ensure t
  :init (setq powerline-default-separator 'arrow)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(use-package ido-vertical-mode
  ;; Ido-mode
  :ensure t
  :init
  (ido-mode 1)
  (setq ido-enable-flex-matching nil
        ido-create-new-buffer 'always
        ido-everywhere t)
  :config (ido-vertical-mode 1))

(use-package smex
  ;; Same but for buffer
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
  :hook ((emacs-lisp-mode scheme-mode) . aggressive-indent-mode))

(use-package linum-relative
  ;; Relative line number
  :ensure t
  :init
  (setq linum-relative-current-symbol "")
  (linum-relative-global-mode))

(column-number-mode 1)  ; Show columns

;; List of buffers to not open in a new window
(add-to-list 'same-window-buffer-names "*Buffer List*")

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(setq-default indent-tabs-mode nil)  ;; No tabs!

(use-package sudo-edit
  :ensure t
  :bind
  ("s-e" . sudo-edit))

(global-auto-revert-mode 1)  ; Automatically update changed buffer

(progn
  ;; Highlight matching brackets
  (show-paren-mode)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package expand-region
  ;; Magic that I saw once.
  :ensure t
  :bind ("C-q" . er/expand-region))

(use-package popup-kill-ring
  ;; Use this to yank multiple things.
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package evil-lion
  ;; Alignment
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-numbers
  ;; Vim Numbering
  :ensure t
  :config
  (global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(add-hook
 ;; Remove completion buffer when done
 'minibuffer-exit-hook
 '(lambda ()
    (let ((buffer "*Completions*"))
      (and (get-buffer buffer)
         (kill-buffer buffer)))))

(desktop-save-mode 1)  ; Save all the buffers to re-open them later.

(setq
 ;; stop creating backup files
 make-backup-files nil
 auto-save-default nil)

(add-hook
 ;; Prettify symbols
 'prog-mode-hook
 (lambda ()
   (setq prettify-symbols-alist
         '(("lambda"  . ?λ)
           ("lam"     . ?λ)
           ("<="      . ?≤)
           (">="      . ?≥)
           ("->"      . ?→)
           ("<-"      . ?←)
           ("<->"     . ?↔)
           ("=>"      . ?➾)
           ("=="      . ?≡)
           ("not"     . ?¬)
           ("=/="     . ?≠)
           ("forall"  . ?∀)
           ("exists"  . ?∃)
           ("compose" . ?∘)
           ("in"      . ?∈)
           ("false"   . ?⊥)
           ("and"     . ?∧)
           ("or"      . ?∨)))))
(global-prettify-symbols-mode 1)

(add-hook
 ;; Delete trailing whitespaces on save.
 'before-save-hook 'delete-trailing-whitespace)

;;; Fix lisp indent
(let ((sif 'scheme-indent-function))
  (put 'lam              sif 'defun)
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
  (put 'while          sif 1))

(global-set-key
 ;; Always kill current buffer
 (kbd "C-x k") 'kill-current-buffer)

;;; Automatic Settings (DON'T TOUCH BEYOND THIS POINT)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sudo-edit spaceline avy smex ido-vertical-mode beacon company evil-numbers evil-lion evil-commentary rainbow-delimiters linum-relative evil-surround evil use-package))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
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
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "navy"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "sienna4"))))
 '(show-paren-match ((t (:underline "cyan" :weight extra-bold)))))
