;;;; INITIAL

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; chill Winston
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;;; CASK

(require 'cask)
(cask-initialize)
(require 'pallet)

;; don't re-load packages
(setq package-enable-at-startup nil)

;;;; GLOBAL

;; use a nice dark theme
(load-theme 'ir-black t)

;; use smart-mode-line
(require 'smart-mode-line)
(add-to-list 'sml/hidden-modes " WS")
(add-to-list 'sml/hidden-modes " FIC")
(add-to-list 'sml/hidden-modes " pair")
(add-to-list 'sml/hidden-modes " ElDoc")
(add-to-list 'sml/hidden-modes " yas")
(add-to-list 'sml/hidden-modes " Projectile")
(add-to-list 'sml/hidden-modes " MRev")
(add-to-list 'sml/hidden-modes " company")
(add-to-list 'sml/hidden-modes " Fly")
(add-to-list 'sml/hidden-modes " GitGutter")
(add-to-list 'sml/hidden-modes " Paredit")
(add-to-list 'sml/hidden-modes " Undo-Tree")
(setq sml/theme 'respectful)
(sml/setup)

;; use Consolas, if available
(set-frame-font "Consolas-12" nil t)

;; use autopair everywhere
(autopair-global-mode t)

;; rely on electric indents, since they're improving
(electric-indent-mode t)

;; use whitespace mode, and mark lines longer than 80 characters
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column 81)
(setq whitespace-global-modes '(not git-commit-mode))

;; add context menus for things
(global-discover-mode t)

;; use line numbers in prog-mode
(add-hook 'prog-mode-hook 'linum-mode)

;; highlight the current line
(global-hl-line-mode)

;; highlight fixme comments
(add-hook 'prog-mode-hook 'fic-mode)

;; always use ElDoc in prog-mode
(add-hook 'prog-mode-hook 'eldoc-mode)

;; enable yasnippet when programming
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; overwrite selections
(delete-selection-mode t)

;; use projectile everywhere
(projectile-global-mode t)

;; group ibuffer by vc root
(add-hook 'ibuffer-hook 'ibuffer-vc-set-filter-groups-by-vc-root)

;; enable flycheck everywhere
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; a tab is 4 spaces wide
(setq-default tab-width 4)

;; don't show the welcome message
(setq inhibit-splash-screen t)

;; shut up shut up shut up
(setq ring-bell-function 'ignore)

;; always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use core-utils for dired
(setq insert-directory-program "gls")

;; always prefer UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; always add a trailing newline - POSIX
(setq require-final-newline t)

;; don't clobber things in the system clipboard when killing
(setq save-interprogram-paste-before-kill t)

;; no need to be so verbose
(defalias 'yes-or-no-p 'y-or-n-p)

;; use undo-tree
(global-undo-tree-mode)

;; bind windmove to super-arrows
(windmove-default-keybindings 'super)

;;;; C/C++

(add-hook 'c-mode-hook 'cppcm-reload-all)
(add-hook 'c++-mode-hook 'cppcm-reload-all)

;;;; IDO

;; use flx-ido
(flx-ido-mode t)

;; use ido vertically
(ido-vertical-mode t)

;; don't complete on extensions with ido
(setq ido-ignore-extensions t)

;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; autoload idomenu
(autoload 'idomenu "idomenu" nil t)

;;;; ELISP

;; surface Elisp sections in imenu
(defun coda/imenu-elisp-sections ()
  (setq imenu-generic-expression '(("Sections" "^;;;; \\(.+\\)" 1)))
  (imenu-add-to-menubar "Index"))
(add-hook 'emacs-lisp-mode-hook 'coda/imenu-elisp-sections)

;; open Cask files in elisp-mode
(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; add support for flychecking Cask files
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-cask-setup))

;; use paredit
(add-hook 'lisp-mode-hook 'paredit-mode)

;;;; COCOA

(defun coda/configure-cocoa ()
  ;; load PATH variable from shell, since setting env bars in Maces
  ;; is crazy painful
  (exec-path-from-shell-initialize)

  ;; open up to 200x60
  (add-to-list 'default-frame-alist '(width . 200))
  (add-to-list 'default-frame-alist '(height . 60))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
)
(if (memq window-system '(mac ns)) (coda/configure-cocoa))

;;;; COMPANY

(require 'company)

;; use a bigger popup window
(setq company-tooltip-limit 20)

;; take over hippie-expand
(defun coda/enable-company-mode ()
  (company-mode 1)
  (define-key (current-local-map) [remap hippie-expand] 'company-complete))
(add-hook 'prog-mode-hook 'coda/enable-company-mode)

;; disable in git-commit-mode since it's crazy annoying
(setq company-global-modes '(not git-commit-mode))

;; strictly limit completion in Go, since it's totally accurate
(defadvice company-go (around fix-company-go-prefix activate)
      ad-do-it
      (when (eql (ad-get-arg 0) 'prefix)
        (setq ad-return-value (company-grab-word))))

;;;; SPELLING

(require 'ispell)

;; use aspell instead of ispell
(setq ispell-program-name "aspell")

;; automatically check spelling for text
(add-hook 'text-mode-hook 'flyspell-mode)

;; spell check comments and strings when programming
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; spell check git commit messages
(add-hook 'git-commit-mode-hook 'flyspell-mode)

;;;; GO

;; hard-code GOROOT and GOPATH for now
(setenv "GOROOT" "/usr/local/go")
(setenv "GOPATH" "/Users/coda/Projects/go")

(require 'go-mode)

;; use goimports instead of gofmt
(setq gofmt-command "goimports")

;; always run goimports before saving .go files
(add-hook 'before-save-hook 'gofmt-before-save)

(defun coda/configure-go-mode ()
  ;; improve imenu results
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  ;; use go-eldoc
  (go-eldoc-setup)

  ;; only use gocode as company backend
  (set (make-local-variable 'company-backends) '(company-go)))

(add-hook 'go-mode-hook 'coda/configure-go-mode)

;;;; MAGIT

;; highlight git changes in the fringe
(global-git-gutter-mode t)

(require 'magit)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; KEYBINDINGS

(global-set-key (kbd "C-c c")       'compile)
(global-set-key (kbd "C-c g")       'magit-status)
(global-set-key (kbd "C-c i")       'idomenu)
(global-set-key (kbd "C-c l p")     'list-packages)
(global-set-key (kbd "C-c r")       'recompile)
(global-set-key (kbd "C-c s")       'projectile-ag)
(global-set-key (kbd "C-c t")       'eshell)
(global-set-key (kbd "C-c +")       'er/expand-region)
(global-set-key (kbd "C-c -")       'er/contract-region)
(global-set-key (kbd "C-c SPC")     'ace-jump-mode)

(global-set-key (kbd "C-c M-x")     'execute-extended-command) ; old M-x

(global-set-key (kbd "M-x")         'smex)
(global-set-key (kbd "M-X")         'smex-major-mode-commands)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)

(defun coda/join-lines ()
  (interactive)
  (join-line -1))
(global-set-key (kbd "C-M-j")         'coda/join-lines)

(defun coda/move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))
(global-set-key (kbd "<C-S-down>")  'coda/move-line-down)

(defun coda/move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))
(global-set-key (kbd "<C-S-up>")    'coda/move-line-up)

;; unmap upcase-region, since it always screws with undo
(global-unset-key (kbd "C-x C-u"))

;;;; CUSTOM

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
