(require 'package)

(when (version< emacs-version "27.0")
  (package-initialize))

(setq package-archives
      ;; All archives should be "http://..."
      (let ((proto "http://"))
        '(("elpy"         . (concat proto "jorgenschaefer.github.io/packages/"))
          ("melpa"        . (concat proto "http://melpa  .org/packages/"))
          ("gnu"          . (concat proto "http://elpa   .gnu   .org/packages/"))
          ("melpa-stable" . (concat proto "http://stable .melpa .org/packages/")))))

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

(progn  ; Getting rid of really annoying buffers
  (setq-default message-log-max nil)
  (kill-buffer "*Messages*")
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
  (evil-define-key 'visual 'global (kbd "H") #'fold-this-sexp)
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
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always)
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
  (add-hook 'skeme-mode-hook (lambda () (aggressive-indent-mode -1))))

(column-number-mode 1)  ; Show columns

(progn  ; List of buffers to not open in a new window
  (add-to-list 'same-window-buffer-names "*Buffer List*")
  (add-to-list 'same-window-buffer-names "*Help*")
  (add-to-list 'same-window-buffer-names "*Quail Completions*"))

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

(add-hook  ; Remove completion buffer when done
 'minibuffer-exit-hook
 '(lambda ()
    (let ((buffer "*Completions*"))
      (and (get-buffer buffer) (kill-buffer buffer)))))

(desktop-save-mode 1)  ; Save current desktop configs on exit

(add-hook 'comint-exec-hook  ; Please don't ask whether to kill running processes
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer)) nil)))

(setq  ; stop making backup files
 make-backup-files nil
 auto-save-default nil)

(use-package math-symbol-lists  ; Unicode and math symbols
  :ensure t
  :config
  (quail-define-package "math" "UTF-8" "Ω" t)
  (mapc  ;; my rules
   (lambda (x)
     (quail-defrule (car x) (cadr x)))
   '(("\\lam" ?λ)
     ("\\==" ?≡) ("\\=/" ?≠)
     ("\\LRa" ?⇔) ("\\<=>" ?⇔) ("\\Ra" ?➾) ("\\=>" ?➾) ("\\ra" ?→) ("\\then" ?→) ("\\->" ?→) ("\\-->" ?⟶) ("\\la" ?←) ("\\<-" ?←) ("\\.<" ?⬸) ("\\.>" ?⤑) ("\\lra" ?↔) ("\\<->" ?↔) ("\\up" ?↑) ("\\down" ?↓) ("\\h->" ?↪) ("\\ul" ?↖) ("\\ur" ?↗) ("\\dl" ?↙) ("\\dr" ?↘) ("\\o<" ?⟲) ("\\refl" ?⟲) ("\\o>" ?⟳) ("\\lla" ?↞) ("\\<<-" ?↞) ("\\rra" ?↠) ("\\trans" ?↠) ("\\->>" ?↠) ("\\lr2" ?⇄) ("\\-><" ?⇄) ("\\symm" ?⇄) ("\\==>" ?⟹) ("\\idem" ?⊸) ("\\-o" ?⊸) ("\\<-|" ?↤) ("\\|->" ?↦)
     ("\\sub" ?⊆) ("\\sup" ?⊇) ("\\supset" ?⊃) ("\\union" ?∪) ("\\inter" ?∩)
     ("\\ex" ?∃) ("\\for" ?∀)
     ("\\<" "⟨⟩") ("\\lang" "⟨⟩")
     ("\\+-" ?±) ("\\<=" ?≤) ("\\>=" ?≥) ("\\=~" ?≅)
     ("\\nat" ?ℕ) ("\\int" ?ℤ) ("\\real" ?ℝ)
     ("\\and" ?∧) ("\\meet" ?∧) ("\\or" ?∨) ("\\join" ?∨) ("\\false" ?⊥) ("\\|=" ?⊨) ("\\|-" ?⊢)
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
  ;; The fonts are: mscr (script), mbfscr (bold script), mfrak (frankfurt), mbf (boldface), Bbb (Double stroke)
  (add-hook 'prog-mode-hook (lambda () (set-input-method 'math)))
  (add-hook 'text-mode-hook (lambda () (set-input-method 'math))))

(let ((sif 'scheme-indent-function))  ; Customize Scheme indent
  (put 'f@             sif 1)
  (put 'forall         sif 1)
  (put '∀              sif 1)
  (put 'exists         sif 1)
  (put '∃              sif 1)
  (put 'go-on          sif 1)
  (put 'let@           sif 1)
  (put 'formula@       sif 1)
  (put 'term@          sif 1)
  (put 'expr@          sif 1)
  (put 'prove          sif 1)
  (put 'lam            sif 1)
  (put 'defun          sif 'defun)
  (put 'set!           sif 1)
  (put 'match          sif 1)
  (put 'match*         sif 1)
  (put 'send           sif 2)
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
  (put 'trace-define-syntax sif 1)
  (put 'pmatch         sif 2)
  (progn  ; miniKanren
    (put 'run*    sif 1)
    (put 'run     sif 2)
    (put 'matche  sif 1)
    (put 'project sif 1)
    (put 'fresh   sif 1))
  (progn  ; Everything else (e.g Skeme)
    (put 'match     sif 1)
    (put 'THE       sif 1)
    (put 'assume    sif 1) (put 'Assume    sif 1)
    (put 'induction sif 1) (put 'Induction sif 1)
    (put 'since     sif 1) (put 'Since     sif 1)
    (put 'theorem   sif 1) (put 'Theorem   sif 1)
    (put 'destruct  sif 1) (put 'Destruct  sif 1)))

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
        (write-region selected-text nil "~/note/graph.dot" nil)
        (call-interactively 'view-graph))
    (message "Select something first!")))

(defun scot ()
  (interactive)
  (if (use-region-p)
      (let ((selected-text (call-interactively 'get-selected-text)))
        (write-region selected-text nil "~/note/graph.scm" nil)
        (shell-command "scheme ~/note/scheme-graph.scm")
        (call-interactively 'view-graph))
    (message "Select something first!")))

(defun view-graph ()
  (interactive)
  (shell-command "dot -Tsvg ~/note/graph.dot -o ~/note/graph.svg")
  (sit-for 1)
  (shell-command "xviewer ~/note/graph.svg"))

;; Binary search
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
  (evil-next-line (/ (- bin-upper bin-lower) 2)))

(progn  ; Key bindings
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
  (evil-define-key 'normal 'global (kbd "TAB") 'evil-indent-line)

  (evil-define-key 'insert 'global (kbd "C-.") 'next-buffer)
  (evil-define-key 'insert 'global (kbd "C-,") 'previous-buffer)
  (evil-define-key 'insert 'global (kbd "C-v") 'evil-paste-before)

  (evil-define-key 'visual 'global (kbd "TAB") 'indent-region)
  (evil-define-key 'visual 'global (kbd ";")   'smex))

(progn  ; Command alias
  (defalias 'k 'kill-buffer-and-window)
  (defalias 'f 'ido-find-file)
  (defalias 'b 'ido-switch-buffer)
  (defalias 'ls 'buffer-menu)
  (defalias 'koq (lambda () (interactive)
                   (find-file "~/note/koq/koq.ss")))
  (defalias 'pov (lambda () (interactive)
                   (find-file "~/note/koq/pov.ss")))
  (defalias 'pie (lambda () (interactive)
                   (find-file "~/note/koq/pie.ss")))
  (defalias 'main (lambda () (interactive)
                    (find-file "~/note/koq/main.ss")))
  (defalias 'config (lambda () (interactive)
                      (find-file  "~/.emacs.d/init.el")))
  (defalias 'thought (lambda () (interactive)
                       (find-file  "~/note/thought.skm")))
  (defalias 'lib (lambda () (interactive)
                   (find-file  "~/note/lib.ss"))))

(progn  ; Pro lisp movements
  (setq evil-move-beyond-eol t) ; The magic is here
  (evil-define-key 'normal 'global (kbd "M-h") 'backward-sexp)
  (evil-define-key 'normal 'global (kbd "M-l") 'forward-sexp)
  (evil-define-key 'normal 'global (kbd "M-k") 'backward-up-list)
  (evil-define-key 'normal 'global (kbd "M-j") 'down-list))

;; (progn  ;;; Proof General and Coq
;;   (setq proof-auto-raise-buffers nil)
;;   (setq proof-multiple-frames-enable nil)
;;   (setq proof-delete-empty-windows nil)
;;   (setq proof-three-window-mode t)


;;   (setq proof-splash-enable nil)  ;; Disable welcome screen
;;   (add-hook 'coq-mode-hook
;;             (lambda ()
;;   (interactive)
;;   (deactivate-input-method)
;;   (kill-all-abbrevs)
;;   (evil-define-key 'normal 'global (kbd "C-n")
;;   (lambda () (interactive)
;;   (proof-assert-next-command-interactive)))
;;   (evil-define-key 'normal 'global (kbd "C-p")
;;   (lambda () (interactive)
;;   (proof-undo-last-successful-command))))))

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
 '(custom-enabled-themes (quote (adwaita)))
 '(package-selected-packages
   (quote
    (fold-this lisp disable-mouse math-symbol-lists rainbow-identifiers spaceline avy smex ido-vertical-mode beacon evil-numbers evil-lion evil-commentary rainbow-delimiters evil-surround evil use-package))))
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
