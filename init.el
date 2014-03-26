(defconst dotfiles-dir
  (file-name-directory
   (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

(require 'init-packages)
(require 'init-common)
(require 'init-cocoa)
(require 'init-company)
(require 'init-flyspell)
(require 'init-go)
(require 'init-keybindings)

(load-theme 'wombat t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
