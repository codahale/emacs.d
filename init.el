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
(when (memq window-system '(mac ns)) ; if we're running as Cocoa
  (progn
    (exec-path-from-shell-initialize) ; load path from shell
    (set-frame-width (selected-frame) 150) ; open up 150x60
    (set-frame-height (selected-frame) 60)))
(setq mouse-wheel-scroll-amount '(1)) ; stop scrolling so damn fast
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-splash-screen t) ; don't show the welcome message

;;; AUTOCOMPLETE SETTINGS
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete) ; bind M-TAB to autocomplete
(ac-config-default)

;;; GO SETTINGS
(setenv "GOROOT" "/usr/local/go") ; make sure we have a GOROOT outside of Terminal
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
