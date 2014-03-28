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

;; unmap upcase-region, since it always screws with undo
(global-unset-key (kbd "C-x C-u"))


(provide 'init-keybindings)
