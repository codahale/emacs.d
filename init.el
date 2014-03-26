;;; init.el -- Coda Hale's special sauce
;;; Commentary:
;;; Code:

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
(projectile-global-mode) ; use projectile everywhere
(flx-ido-mode 1) ; use flx-ido
(ido-vertical-mode 1) ; use ido vertically
(add-hook 'after-init-hook #'global-flycheck-mode) ; use flycheck everywhere

;;; COMMON SETTINGS
(setq-default tab-width 4) ; a tab is 4 spaces
(setq inhibit-splash-screen t) ; don't show the welcome message
(setq ring-bell-function 'ignore) ; shut up shut up shut up
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; trim everything
(add-hook 'prog-mode-hook ; mark 81 char columns in prog-mode
          (lambda () (interactive) (column-marker-1 81)))
(setq ido-ignore-extensions t)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights.
(setq insert-directory-program "gls") ; use core-utils for dired

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

;;; COMPANY SETTINGS
(require 'company) ; load company mode
(setq company-tooltip-limit 20) ; bigger popup window
(setq company-minimum-prefix-length 0) ; autocomplete right after '.'
(setq company-idle-delay .3) ; shorter delay before autocompletion popup
(setq company-echo-delay 0) ; removes annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(custom-set-faces ; make company a little less ugly
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))

;;; GO SETTINGS
(add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on save
(add-hook 'go-mode-hook
          '(lambda ()
             ; improve imenu results
             (setq imenu-generic-expression
                   '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
                     ("func" "^func *\\(.*\\) {" 1)))
             (imenu-add-to-menubar "Index")
             ; always indent after a return
             (define-key go-mode-map (kbd "RET") #'go-mode-insert-and-indent)
             ; use goimports to auto-import/trim
             (setq gofmt-command "goimports")
             ; use go-eldoc
             (go-eldoc-setup)
             ; only use gocode as company backend
             (set (make-local-variable 'company-backends) '(company-go))
             ; enable company-mode
             (company-mode)
))

;;; GIT SETTINGS
(add-hook 'git-commit-mode-hook 'flyspell-mode) ; enable Flyspell

;;; THEME SETTINGS
(load-theme 'wombat t)

;;; KEY BINDINGS
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c i") 'imenu-anywhere)
(global-set-key (kbd "C-c <up>") 'er/expand-region)
(global-set-key (kbd "C-c <down>") 'er/contract-region)
(global-set-key (kbd "C-c s") 'ag-project)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command) ; old M-x

;;; CUSTOMIZATIONS
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
