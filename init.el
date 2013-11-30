;;; PACKAGES
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;; COMMON SETTINGS
(setq-default tab-width 4)
(setq ring-bell-function 'ignore) ; shut up shut up shut up
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)) ; load path from shell
(setq mouse-wheel-scroll-amount '(1))
(setq mouse-wheel-progressive-speed nil)


;;; AUTOCOMPLETE SETTINGS
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete) ; bind M-TAB to autocomplete
(ac-config-default)

;;; GO SETTINGS
(add-to-list 'load-path "~/Projects/go/src/github.com/nsf/gocode") ; use gocode for autocomplete
(require 'go-autocomplete)
(add-to-list 'ac-modes 'go-mode)

(add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on save
(add-hook 'go-mode-hook 'go-eldoc-setup) ; run go-eldoc when in go-mode
(add-hook 'go-mode-hook (lambda () (interactive) (column-marker-1 81))) ; mark 81 char columns in Go mode

(add-to-list 'load-path "~/Projects/go/src/github.com/dougm/goflymake") ; use goflymake
(require 'go-flymake)

(define-key go-mode-map (kbd "RET") #'go-mode-insert-and-indent) ; stop crapping up returns
(add-hook 'go-mode-hook 'flyspell-prog-mode) ; enable Flyspell for Go comments and strings

;;; GIT SETTINGS
(add-hook 'git-commit-mode-hook 'flyspell-mode) ; enable Flyspell in git-commit-mode, requires ispell

;;; THEME SETTINGS
(load-theme 'wombat t)

;;; KEY BINDINGS
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x r") 'recompile)
