(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

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

; Tabs
(use-package evil-tabs
  :ensure t
  :config
  (global-evil-tabs-mode t))


; Adding new file extension to programming mode
(add-to-list 'auto-mode-alist '("\\.kar\\'" . prog-mode))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . prog-mode))


; Make movement keys work respect visual lines
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

(setq-default
  indent-tabs-mode nil
  tab-width 4
  tab-stop-list (quote (4 8)))

; Get rid of the menu bar
(menu-bar-mode -1)

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



; Key binding
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-normal-state-map "a" 'evil-append-line)
(define-key evil-normal-state-map "p" 'evil-paste-before)
(define-key evil-motion-state-map [down] 'evil-scroll-line-down)
(define-key evil-motion-state-map [up] 'evil-scroll-line-up)
(define-key evil-motion-state-map (kbd "RET") 'evil-write-all)
(evil-define-key 'normal 'global [right] 'next-buffer)
(evil-define-key 'normal 'global [left] 'previous-buffer)
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
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("29c71db1bc8738c0270e2751babc5cd39d82e496e912c8afc04d59a12429e3bb" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(match ((t (:background "RoyalBlue3" :underline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "white"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "brightgreen"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "color-63"))))
 '(show-paren-match ((t (:underline "cyan" :weight extra-bold)))))
