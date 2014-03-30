;;;; INITIAL

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; chill Winston
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;;; CASK

(require 'cask "/usr/local/Cellar/cask/0.6.0/cask.el")
(cask-initialize)
(require 'pallet)

;;;; GLOBAL

;; use a nice dark theme
(load-theme 'wombat t)
(powerline-default-theme)

;; use autopair everywhere
(autopair-global-mode)

;; use whitespace mode, and mark lines longer than 80 characters
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column 81)
(setq whitespace-global-modes '(not git-commit-mode))

;; add context menus for things
(global-discover-mode 1)

;; use line numbers in prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; highlight the current line number
(add-hook 'prog-mode-hook 'hlinum-activate)

;; highlight fixme comments
(add-hook 'prog-mode-hook 'fic-mode)

;; enable yasnippet everywhere
(yas-global-mode t)

;; overwrite selections
(delete-selection-mode t)

;; use projectile everywhere
(projectile-global-mode t)

;; enable flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; a tab is 4 spaces wide
(setq-default tab-width 4)

;; don't show the welcome message
(setq inhibit-splash-screen t)

;; shut up shut up shut up
(setq ring-bell-function 'ignore)

;; always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use core-utils for dired
(setq insert-directory-program "gls")

;; always prefer UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; always add a trailing newline - POSIX
(setq require-final-newline t)

;; don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

;; prompt when vcs state is dirty on exit
(vc-check-status-activate 1)

;;;; LEGALESE

(require 'legalese)
(custom-set-variables
  '(legalese-default-author "Coda Hale")
  '(legalese-default-copyright "coda.hale@gmail.com"))

;;;; IDO

;; use flx-ido
(flx-ido-mode t)

;; use ido vertically
(ido-vertical-mode t)

;; don't complete on extensions with ido
(setq ido-ignore-extensions t)

;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;;;; FLYCHECK

;; make sure the flycheck errors are visible
(custom-set-faces '(flycheck-fringe-error ((t (:background "dark red")))))

;; ignore all the elisp doc linting
(custom-set-variables
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc ruby-rubylint))))

;;;; ELISP

;; surface Elisp sections in imenu
(defun coda/imenu-elisp-sections ()
  (setq imenu-generic-expression '(("Sections" "^;;;; \\(.+\\)" 1)))
  (imenu-add-to-menubar "Index"))
(add-hook 'emacs-lisp-mode-hook 'coda/imenu-elisp-sections)

;; open Cask files in elisp-mode
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; add support for flychecking Cask files
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;;;; COCOA

(defun coda/configure-cocoa ()
  ;; load PATH variable from shell, since setting env bars in Maces
  ;; is crazy painful
  (exec-path-from-shell-initialize)

  ;; open up to 200x60
  (add-to-list 'default-frame-alist '(width . 200))
  (add-to-list 'default-frame-alist '(height . 60))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
)
(if (memq window-system '(mac ns)) (coda/configure-cocoa))

;;;; COMPANY

(require 'company)

;; use a bigger popup window
(setq company-tooltip-limit 20)

;; shorten the delay before showing the popup
(setq company-idle-delay .3)

;; eliminate weird blinking
(setq company-echo-delay 0)

;; only start autocompletion after typing
(setq company-begin-commands '(self-insert-command))

;; take over hippie-expand
(defun coda/enable-company-mode ()
  (company-mode 1)
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))
(add-hook 'prog-mode-hook 'coda/enable-company-mode)

;; disable in git-commit-mode since it's crazy annoying
(setq company-global-modes '(not git-commit-mode))

;; strictly limit completion in Go, since it's totally accurate
(defadvice company-go (around fix-company-go-prefix activate)
      ad-do-it
      (when (eql (ad-get-arg 0) 'prefix)
        (setq ad-return-value (company-grab-word))))

;; make company not look like hammered shit
(custom-set-faces
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(company-scrollbar-bg ((t (:foreground "darkgray" :background "lightgray"))))
 '(company-scrollbar-fg ((t (:background "darkgray" :foreground "lightgray")))))

;;;; SPELLING

(require 'ispell)

;; use aspell instead of ispell
(setq ispell-program-name "ispell")

;; automatically check spelling for text
(add-hook 'text-mode-hook 'flyspell-mode)

;; spell check comments and strings when programming
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; spell check git commit messages
(add-hook 'git-commit-mode-hook 'flyspell-mode)

;;;; GO

;; hard-code GOROOT and GOPATH for now
(setenv "GOROOT" "/usr/local/go")
(setenv "GOPATH" "/Users/coda/Projects/go")

(require 'go-mode)

;; use goimports instead of gofmt
(setq gofmt-command "goimports")

;; always run goimports before saving .go files
(add-hook 'before-save-hook 'gofmt-before-save)

(defun coda/configure-go-mode ()
  ;; improve imenu results
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; always indent after a return
  (define-key go-mode-map (kbd "RET") #'go-mode-insert-and-indent)

  ;; use go-eldoc
  (go-eldoc-setup)

  ;; only use gocode as company backend
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook 'coda/configure-go-mode)

;;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; KEYBINDINGS

(global-set-key (kbd "C-c c")      'compile)
(global-set-key (kbd "C-c g")      'magit-status)
(global-set-key (kbd "C-c i")      'imenu-anywhere)
(global-set-key (kbd "C-c r")      'recompile)
(global-set-key (kbd "C-c s")      'projectile-ag)
(global-set-key (kbd "C-c <up>")   'er/expand-region)
(global-set-key (kbd "C-c <down>") 'er/contract-region)
(global-set-key (kbd "C-c SPC")    'ace-jump-mode)

(global-set-key (kbd "C-c M-x")    'execute-extended-command) ; old M-x

(global-set-key (kbd "M-x")        'smex)
(global-set-key (kbd "M-X")        'smex-major-mode-commands)

;; unmap upcase-region, since it always screws with undo
(global-unset-key (kbd "C-x C-u"))

;;;; CUSTOM

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
