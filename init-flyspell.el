(add-hook 'prog-mode-hook 'flyspell-prog-mode) ; spell check comments and strings
(add-hook 'text-mode-hook 'flyspell-mode) ; enable Flyspell for text
(add-hook 'git-commit-mode-hook 'flyspell-mode) ; also for git commits

(provide 'init-flyspell)
