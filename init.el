;;; PACKAGES
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;; GLOBAL MODES
(electric-pair-mode +1) ; use electric pairs
(column-number-mode +1) ; use column number mode
(yas-global-mode +1) ; enable yasnippets everywhere
(delete-selection-mode t) ; overwrite selections

;;; COMMON SETTINGS
(setq-default tab-width 4) ; a tab is 4 spaces
(setq inhibit-splash-screen t) ; don't show the welcome message
(setq ring-bell-function 'ignore) ; shut up shut up shut up
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; trim everything
(add-hook 'prog-mode-hook ; mark 81 char columns in prog-mode
          (lambda () (interactive) (column-marker-1 81)))
(setq ido-ignore-extensions t)

;;; SPEEDBAR
(sr-speedbar-open) ; automatically open sr-speedbar
(speedbar-add-supported-extension ".go")
(speedbar-add-supported-extension ".md")


;;; COCOA SETTINGS
(when (memq window-system '(mac ns))
  (progn
    (exec-path-from-shell-initialize) ; load path from shell
    (add-to-list 'default-frame-alist '(width . 200)) ; open up to 200x60
    (add-to-list 'default-frame-alist '(height . 60))
    (setq mouse-wheel-scroll-amount '(1)) ; stop scrolling so damn fast
    (setq mouse-wheel-progressive-speed nil)
    (setenv "GOROOT" "/usr/local/go") ; use the pkg-installed GOROOT
    (setenv "GOPATH" "/Users/coda/Projects/go") ; ok no for real
    ))

;;; FLYSPELL SETTINGS
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ; spell check comments and strings
(add-hook 'text-mode-hook 'flyspell-mode) ; enable Flyspell for text

;;; AUTOCOMPLETE SETTINGS
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-delay 0.5) ; wait a half-second before chipping in

;;; GO SETTINGS
(setq gofmt-command "goimports") ; use goimports to auto-import/trim
(add-to-list 'load-path "~/Projects/go/src/github.com/nsf/gocode")
(require 'go-autocomplete)
(add-to-list 'ac-modes 'go-mode)

(add-hook 'before-save-hook ; run gofmt on save
          'gofmt-before-save)
(add-hook 'go-mode-hook ; run go-eldoc when in go-mode
          'go-eldoc-setup)

(add-to-list 'load-path "~/Projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake)

(define-key go-mode-map (kbd "RET") #'go-mode-insert-and-indent)

; improve imenu results
(add-hook 'go-mode-hook '(lambda ()
                           (setq imenu-generic-expression
                                 '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
                                   ("func" "^func *\\(.*\\) {" 1)))
                           (imenu-add-to-menubar "Index")))

;;; GIT SETTINGS
(add-hook 'git-commit-mode-hook 'flyspell-mode) ; enable Flyspell

;;; THEME SETTINGS
(load-theme 'wombat t)

;;; KEY BINDINGS
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c i") 'imenu-anywhere)
(global-set-key (kbd "C-c p") 'package-list-packages)
(global-set-key (kbd "C-c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c <up>") 'er/expand-region)
(global-set-key (kbd "C-c <down>") 'er/contract-region)
(global-set-key (kbd "C-c s") 'ag-project)
(global-set-key (kbd "C-c m") 'sr-speedbar-toggle)

;;; CUSTOMIZED CRAP
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" "_workspace/" ".test"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
