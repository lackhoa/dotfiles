(require 'package)
(setq package-archives
      '(("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-refresh-contents)
(package-initialize)
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

; Relative line number
(use-package linum-relative
  :ensure t
  :config
  (linum-relative-on))

; Powerline
(use-package powerline
  :ensure t
  :config
  (powerline-center-evil-theme))

; Rainbow
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

; No tabs!
(setq-default indent-tabs-mode nil)

; Show line number
(global-linum-mode t)
(setq linum-relative-current-symbol "")

; Highlight matching brackets
(show-paren-mode)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

; Custome theme or whatever
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

; Auto-pair
(electric-pair-mode)

; Commentary
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

; The (legendary) sniping ability
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode 1))

; Alignment
(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

; Adding new file extension to modes
(add-to-list 'auto-mode-alist '("\\.kar\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . scheme-mode))


; Make movement keys work respect visual lines
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

; Get rid of the UI elements
(menu-bar-mode -1)
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 

; Vim Numbering
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

; Auto completion
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.2)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
; Delete word when in automcomplete
(with-eval-after-load 'company
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word))
(with-eval-after-load 'helm
    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

; Kill the buffer with ':x'
(evil-ex-define-cmd "x" 'kill-this-buffer)
(evil-ex-define-cmd "f" 'find-file)

; stop creating backup~ files
(setq make-backup-files nil)
; stop creating #autosave# files
(setq auto-save-default nil)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Don't show Welcome Screen when opening up
(setq inhibit-startup-screen t)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

(load-theme 'wheatgrass)

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

; Ignore case in minibuffer's tab completion
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)

; Maximize on start-up
(add-to-list 'default-frame-alist '(fullscreen . maximized))

; Turn off the annoying reindenting
(electric-indent-mode -1)

; Prettify symbols
(add-hook 'prog-mode-hook
  (lambda ()
    (setq prettify-symbols-alist
          '(("lambda" . ?λ)
            ("lam"    . ?λ)
            ("<="     . ?≤)
            (">="     . ?≥)
            ("->"     . ?→)
            ("=>"     . ?➾)))))
(global-prettify-symbols-mode 1)








; Key binding
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-normal-state-map "a" 'evil-append-line)
(define-key evil-normal-state-map "p" 'evil-paste-before)
(evil-define-key 'insert 'global (kbd "C-v") 'evil-paste-before)
(define-key evil-normal-state-map "P" 'evil-paste-after)
(define-key evil-motion-state-map (kbd "RET") 'evil-write-all)
(evil-define-key 'normal 'global [down] 'evil-scroll-line-down)
(evil-define-key 'insert 'global [down] 'evil-scroll-line-down)
(evil-define-key 'normal 'global [up] 'evil-scroll-line-up)
(evil-define-key 'insert 'global [up] 'evil-scroll-line-up)
(evil-define-key 'normal 'global [right] 'next-buffer)
(evil-define-key 'insert 'global [right] 'next-buffer)
(evil-define-key 'normal 'global [left] 'previous-buffer)
(evil-define-key 'insert 'global [left] 'previous-buffer)
(evil-define-key 'normal 'global (kbd "C-j") #'add-line-below)
(defun add-line-below ()
  (interactive) (evil-open-below 0)
  (evil-normal-state) (evil-previous-line) (evil-first-non-blank))

(evil-define-key 'normal 'global (kbd "C-k") #'add-line-above)
(defun add-line-above ()
  (interactive) (evil-open-above 0)
  (evil-normal-state) (evil-next-line) (evil-first-non-blank))







(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 203 :width normal))))
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
