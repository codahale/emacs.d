(autopair-global-mode) ; use autopair everywhere
(column-number-mode t) ; use column number mode
(yas-global-mode t) ; enable yasnippets everywhere
(delete-selection-mode t) ; overwrite selections
(projectile-global-mode t) ; use projectile everywhere
(flx-ido-mode t) ; use flx-ido
(ido-vertical-mode t) ; use ido vertically
(add-hook 'after-init-hook #'global-flycheck-mode) ; use flycheck everywhere

(setq-default tab-width 4) ; a tab is 4 spaces
(setq inhibit-splash-screen t) ; don't show the welcome message
(setq ring-bell-function 'ignore) ; shut up shut up shut up
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; trim everything
(add-hook 'prog-mode-hook ; mark 81 char columns in prog-mode
          (lambda () (interactive) (column-marker-1 81)))
(add-hook 'prog-mode-hook 'linum-mode) ; use line numbers in prog-mode
(setq ido-ignore-extensions t)
(setq ido-use-faces nil) ; disable ido faces to see flx highlights.
(setq insert-directory-program "gls") ; use core-utils for dired

;; always prefer UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; always add a trailing newline - POSIX
(setq require-final-newline t)

;; Don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

(provide 'init-common)
