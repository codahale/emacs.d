(require 'company) ; load company mode
(setq company-tooltip-limit 20) ; bigger popup window
(setq company-idle-delay .3) ; shorter delay before autocompletion popup
(setq company-echo-delay 0) ; removes annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(defun coda/enable-company-mode ()
  (company-mode 1)
  ;; Make sure emacs does the right thing with completion command
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))

(add-hook 'prog-mode-hook 'coda/enable-company-mode)
(setq company-global-modes '(not git-commit-mode)) ; never git-commit

(provide 'init-company)
