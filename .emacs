(require 'package)
(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Version 27.0 automatically initializes packages for you
(when (version< emacs-version "27.0")
  (package-initialize))

;; Ignore case in minibuffer's tab completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

;; Maximize on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Load THE theme
(load-theme 'wheatgrass)

;; Don't skip the screen when scrolling up or down
(setq scroll-conservatively 100)

;; Something really annoying?
(setq ring-bell-function 'ignore)

;; Show where the cursor is
(global-hl-line-mode)

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
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Auto-pair
(electric-pair-mode)


;;; Packages
(require 'use-package)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))
  (use-package evil-indent-textobject
    :ensure t))

;; Switch line highlighting off when in insert mode.
(add-hook 'evil-insert-state-entry-hook
          '(lambda () (global-hl-line-mode -1)))
(add-hook 'evil-normal-state-entry-hook
          '(lambda () (global-hl-line-mode)))

;; Ido-mode
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex))

(use-package avy
  :ensure t)

;; Highlight the cursor a bit when switching buffer
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook
            #'aggressive-indent-mode)
  (add-hook 'scheme-mode-hook
            #'aggressive-indent-mode))

;; Surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

;; Relative line number
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-on))

;; Show column
(column-number-mode 1)

;; Rainbow
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; No tabs!
(setq-default indent-tabs-mode nil)

;; Show line number
(global-linum-mode t)
(setq linum-relative-current-symbol "")

;; Highlight matching brackets
(show-paren-mode)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;; Commentary
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

;; The (legendary) sniping ability
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode 1))

;; Alignment
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

;; Vim Numbering
(use-package evil-numbers
  :ensure t
  :config
  (global-set-key (kbd "C-a") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Auto completion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
;; Delete word when in automcomplete
(with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))
(with-eval-after-load 'helm
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

;; A bunch of commands
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
      (replace-match replacement))))

;; stop creating backup~ files
(setq make-backup-files nil)
;; stop creating #autosave# files
(setq auto-save-default nil)



;; Highlight indentation
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'column)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))



;; Prettify symbols
(add-hook 'prog-mode-hook
          (lambda ()
            (setq prettify-symbols-alist
                  '(("lambda" . ?λ)
                    ("lam"    . ?λ)
                    ("<="     . ?≤)
                    (">="     . ?≥)
                    ("->"     . ?→)
                    ("<-"     . ?←)
                    ("<->"    . ?↔)
                    ("=>"     . ?➾)
                    ("=="     . ?≡)
                    ("=/="    . ?≠)))))
(global-prettify-symbols-mode 1)

;; Delete trailing whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Fix lisp indent
(let ((sif 'scheme-indent-function))
  (put 'lam         sif 'defun)
  (put 'def       sif 1)
  (put 'class     sif 1)
  (put 'class*    sif 2)
  (put 'match     sif 1)
  (put 'match*    sif 1)
  (put 'send      sif 2)
  (put 'for       sif 1)
  (put 'let/cc    sif 1)
  (put 'let/ec    sif 1)
  (put 'trace-let sif 2)
  (put 'apply     sif 1))


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
;; Avy for the win!
(evil-define-key 'normal 'global (kbd "f") #'avy-goto-char)

;;; Automatic Settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy smex ido-vertical-mode beacon "use-package" highlight-indent-guides company evil-numbers evil-lion evil-snipe evil-commentary rainbow-delimiters linum-relative evil-surround evil-indent-textobject evil-leader evil use-package))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 203 :width normal))))
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
