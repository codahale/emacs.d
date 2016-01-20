;;; load custom settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; bootstrap `use-package'
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;; store autosave files in tmp
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(set-terminal-coding-system 'utf-8)     ; always use UTF-8
(set-keyboard-coding-system 'utf-8)     ; it is the future
(prefer-coding-system 'utf-8)

(setq ring-bell-function 'ignore)       ; don't blink constantly
(add-hook 'before-save-hook
          'delete-trailing-whitespace)  ; always delete trailing whitespace
(setq insert-directory-program "gls")   ; use core-utils on Mac
(defalias 'yes-or-no-p 'y-or-n-p)       ; accept "y" for "yes"

(defun coda/configure-cocoa ()
  ;; open up maximized-ish
  (let ((px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty)
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'default-frame-alist (cons 'width tx))
    (add-to-list 'default-frame-alist (cons 'height ty)))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))
(if (memq window-system '(mac ns)) (coda/configure-cocoa))

(electric-pair-mode t)                  ; automatically pair quotes and such
(global-hl-line-mode)                   ; highlight the current line
(delete-selection-mode t)               ; delete selections when yanking etc
(windmove-default-keybindings 'super)   ; bind windmove to s-{arrows}

;;; prog-mode specifics
(add-hook 'prog-mode-hook 'linum-mode)  ; show line numbers
(add-hook 'prog-mode-hook 'column-number-mode) ; show column numbers
(add-hook 'prog-mode-hook 'eldoc-mode)            ; always use eldoc

;;; use whitespace mode, and mark lines longer than 80 characters
(global-whitespace-mode)

;;; automatically check spelling
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key (kbd "C-c c")   'compile)
(global-set-key (kbd "C-c l p") 'list-packages)
(global-set-key (kbd "C-c r")   'recompile)

;;; Packages!

(use-package paredit
  :ensure t
  :config
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'sql-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'web-mode))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t))

(use-package better-defaults
  :ensure t)

(use-package browse-at-remote
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (setq cider-repl-use-clojure-font-lock t)
  (setq cider-repl-use-pretty-printing t)
  (set-variable 'cider-prompt-for-symbol nil)

  ;; treat Schema defn/fn as regular defn/fn
  (put-clojure-indent 's/defn :defn)
  (put-clojure-indent 's/fn :defn)
  (put 's/defn 'clojure-doc-string-elt 2)
  (put 's/defschema 'clojure-doc-string-elt 2)

  (defun cider-repl-reset ()
    (interactive)
    (save-some-buffers)
    (cider-interactive-eval
     (concat "((or (resolve 'user/reset)"
             "     (resolve 'clojure.tools.namespace.repl/refresh)))")))

  (define-key cider-mode-map (kbd "C-c C-x") 'cider-repl-reset)
  (define-key clojure-mode-map (kbd "C-c C-x") 'cider-repl-reset))

(use-package cljr-helm
  :ensure t
  :config
  (define-key clojure-mode-map (kbd "C-c r") 'cljr-helm))

(use-package clojure-cheatsheet
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (defun coda-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'coda-clojure-mode-hook))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package company
  :ensure t
  :config
  (define-key company-mode-map [remap hippie-expand] 'company-complete)
  (define-key company-active-map [remap hippie-expand] 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-web
  :ensure t)

(use-package cpputils-cmake
  :ensure t
  :config
  (add-hook 'c-mode-hook 'cppcm-reload-all)
  (add-hook 'c++-mode-hook 'cppcm-reload-all))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package discover
  :ensure t
  :config
  (global-discover-mode t))

(use-package discover-clj-refactor
  :ensure t)

(use-package emoji-cheat-sheet-plus
  :ensure t)

(use-package enh-ruby-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t)

(use-package fic-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package flyspell-lazy
  :ensure t)

(use-package git-commit
  :ensure t
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode))

(use-package go-mode
  :ensure t)

(use-package helm
  :ensure t
  :config
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (require 'helm-config)

  (global-set-key (kbd "C-c M-x")     'execute-extended-command) ; old M-x
  (global-set-key (kbd "C-x C-d")     'helm-browse-project)
  (global-set-key (kbd "C-h C-f")     'helm-apropos)
  (global-set-key (kbd "C-h r")       'helm-info-emacs)
  (global-set-key (kbd "C-h i")       'helm-info-at-point)
  (global-set-key (kbd "C-:")         'helm-eval-expression-with-eldoc)
  (global-set-key (kbd "C-,")         'helm-calcul-expression)
  (global-set-key (kbd "C-x C-b")     'helm-buffers-list)
  (global-set-key (kbd "C-c f")       'helm-recentf)
  (global-set-key (kbd "C-x C-f")     'helm-find-files)
  (global-set-key (kbd "M-x")         'helm-M-x)
  (global-set-key (kbd "M-y")         'helm-show-kill-ring)
  (global-set-key (kbd "C-c i")       'helm-imenu)
  (global-set-key (kbd "C-x b")       'helm-mini)
  (global-set-key (kbd "C-x C-f")     'helm-find-files)
  (global-set-key (kbd "C-c h o")     'helm-occur)

  (define-key global-map [remap jump-to-register]      'helm-register)
  (define-key global-map [remap list-buffers]          'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
  (define-key global-map [remap find-tag]              'helm-etags-select)
  (define-key global-map [remap xref-find-definitions] 'helm-etags-select)

  (helm-adaptive-mode t)
  (helm-mode 1))

(use-package helm-ag
  :ensure t)

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package helm-flycheck
  :ensure t
  :config
  (eval-after-load 'flycheck
    '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'helm-swoop))

(use-package highlight-parentheses
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

(use-package inf-ruby
  :ensure t)

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package json-mode
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

(use-package markdown-mode
  :ensure t)

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rich-minority
  :ensure t
  :config
  (setq rm-blacklist (quote (" WS"
                             " FIC"
                             " pair"
                             " ElDoc"
                             " yas"
                             " Projectile"
                             " MRev"
                             " company"
                             " Fly"
                             " Undo-Tree"
                             " Anzu"
                             " hl-s"
                             " VHl"
                             " HI"
                             " HI2"
                             " Abbrev"
                             " Interactive"
                             " Helm"
                             " WK"
                             " SP"
                             " =>"
                             " Paredit"
                             " hl-p"
                             " cljr"))))

(use-package rust-mode
  :ensure t)

(use-package scala-mode
  :ensure t)

(use-package shell-pop
  :ensure t)

(use-package slamhound
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smooth-scroll
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 5))

(use-package toml-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  ;; add navigation to Soy templates
  (add-to-list 'web-mode-imenu-regexp-list
               '("^{\\(template\\)[ ]+\\([^ ]+\\).*$" 1 2 " "))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.soy\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package yaml-mode
  :ensure t)
