(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ad-redefinition-action (quote accept))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(cider-repl-display-help-banner nil)
 '(cider-repl-use-pretty-printing t)
 '(custom-safe-themes
   (quote
    ("a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "ce557950466bf42096853c6dac6875b9ae9c782b8665f62478980cc5e3b6028d" "4f2ede02b3324c2f788f4e0bad77f7ebc1874eff7971d2a2c9b9724a50fb3f65" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f5eb916f6bd4e743206913e6f28051249de8ccfd070eae47b5bde31ee813d55f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "95a6ac1b01dcaed4175946b581461e16e1b909d354ada79770c0821e491067c6" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "6fe6ab4abe97a4f13533e47ae59fbba7f2919583f9162b440dd06707b01f7794" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" default)))
 '(fill-column 80)
 '(flycheck-disabled-checkers
   (quote
    (emacs-lisp-checkdoc ruby-rubylint clojure-cider-typed clojure-cider-eastwood)))
 '(gc-cons-threshold 10000000)
 '(helm-adaptive-history-file "~/.emacs.d/helm-history")
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-transformer-show-only-basename nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-scroll-amount 8)
 '(helm-split-window-in-side-p t)
 '(imenu-auto-rescan t)
 '(inhibit-startup-echo-area-message t)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(magit-push-always-verify nil)
 '(neo-smart-open t)
 '(neo-theme (quote nerd))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(package-enable-at-startup nil)
 '(projectile-completion-system (quote helm))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" "build" "Godeps")))
 '(recentf-max-saved-items 200)
 '(require-final-newline t)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "C-t")
 '(tab-width 4)
 '(visible-bell t)
 '(whitespace-global-modes (quote (not git-commit-mode)))
 '(whitespace-style (quote (face empty lines-tail trailing))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro")))))
