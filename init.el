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

;;; GO SETTINGS
(add-to-list 'load-path "/usr/local/Cellar/go/1.2rc3/libexec/misc/emacs/") ; use homebrew mode
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save) ; run gofmt on save

(add-to-list 'load-path "~/Projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake) ; enable Flymake for Go

;;; GIT SETTINGS
(add-hook 'git-commit-mode-hook 'flyspell-mode-on) ; enable Flyspell in git-commit-mode
