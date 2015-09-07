;;;; INITIALIZATION

(setq-default gc-cons-threshold 10000000) ; trade memory for speed

(require 'cask)                         ; load cask & initialize packages
(cask-initialize)
(package-initialize)

(require 'pallet)
(pallet-mode t)                         ; manage all packages via cask

(setq custom-file "~/.emacs.d/custom.el") ; load custom settings
(load custom-file 'noerror)

;;;; GLOBAL

(setq ns-use-srgb-colorspace t)           ; don't look like crap on Mac
(load-theme 'zenburn t)                   ; use zenburn theme

(setq inhibit-splash-screen t)          ; don't show the splash screen
(setq inhibit-startup-message t)        ; don't show startup messages
(setq inhibit-startup-echo-area-message t) ; don't echo anything

(set-terminal-coding-system 'utf-8)     ; always use UTF-8
(set-keyboard-coding-system 'utf-8)     ; it is the future
(prefer-coding-system 'utf-8)

(setq-default tab-width 4)              ; a tab is 4 spaces
(setq ring-bell-function 'ignore)       ; don't blink constantly
(add-hook 'before-save-hook
          'delete-trailing-whitespace)  ; always delete trailing whitespace
(setq insert-directory-program "gls")   ; use core-utils on Mac
(setq require-final-newline t)          ; always add a final newline
(defalias 'yes-or-no-p 'y-or-n-p)       ; accept "y" for "yes"

(require 'rich-minority)                ; don't show all the damn minor modes
(setq rm-blacklist (quote (
                           " WS"
                           " FIC"
                           " pair"
                           " ElDoc"
                           " yas"
                           " Projectile"
                           " MRev"
                           " company"
                           " Fly"
                           " Undo-Tree"
                           " Anzu"
                           " hl-s"
                           " VHl"
                           " HI"
                           " HI2"
                           " Abbrev"
                           " Interactive"
                           " Helm"
                           " WK"
                           " SP"
                           " =>"
                           " Paredit"
                           )))

(require 'smart-mode-line)
(setq sml/theme 'respectful)            ; make smart-mode-line respect the theme
(sml/setup)

(require 'yasnippet)
(setq yas-verbosity 0)                  ; tone down yasnippet logging
(yas-global-mode)                       ; use yasnippet everywhere

(global-diff-hl-mode)                   ; highlight uncommitted changes
(which-key-mode)                        ; display help for partial key bindings
(require 'popwin) (popwin-mode 1)       ; manage temporary windows
(global-anzu-mode t)                    ; show total # of matches in modeline
(autopair-global-mode t)                ; autopair everywhere
(electric-indent-mode t)                ; auto-indent things
(global-discover-mode t)                ; add contextual menus for things
(global-hl-line-mode)                   ; highlight the current line
(delete-selection-mode t)               ; delete selections when yanking etc
(projectile-global-mode t)              ; use projectile when possible
(global-aggressive-indent-mode t)       ; always aggressively indent
(global-undo-tree-mode)                 ; use undo-tree
(windmove-default-keybindings 'super)   ; bind windmove to s-{arrows}
(setq ad-redefinition-action 'accept)   ; stop logging weird crap

;; prog-mode specifics
(add-hook 'prog-mode-hook 'linum-mode)  ; show line numbers
(add-hook 'prog-mode-hook 'column-number-mode) ; show column numbers
(add-hook 'prog-mode-hook 'fic-mode)           ; highlight TODOs
(add-hook 'prog-mode-hook 'highlight-symbol-mode) ; highlight current symbol
(add-hook 'prog-mode-hook 'eldoc-mode)            ; always use eldoc
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; enable rainbow delimiters

;; use whitespace mode, and mark lines longer than 80 characters
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column 80)
(setq whitespace-global-modes '(not git-commit-mode))

;; also fill paragraphs to 80 characters
(setq-default fill-column 80)
(setq-default whitespace-line-column 80)

(defun coda/set-rust-fill-column ()
  (setq fill-column 100)
  (setq whitespace-line-column 100))
(add-hook 'rust-mode-hook 'coda/set-rust-fill-column)

;; enable flycheck everywhere
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

;;;; C/C++

(add-hook 'c-mode-hook 'cppcm-reload-all)
(add-hook 'c++-mode-hook 'cppcm-reload-all)

;;;; ELISP

;; surface Elisp sections in imenu
(defun coda/imenu-elisp-sections ()
  (setq imenu-generic-expression '(("Sections" "^;;;; \\(.+\\)" 1)))
  (imenu-add-to-menubar "Index"))
(add-hook 'emacs-lisp-mode-hook 'coda/imenu-elisp-sections)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

;;;; HELM

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-net)
(require 'projectile)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq helm-split-window-in-side-p            t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf  t
      helm-ff-transformer-show-only-basename nil
      helm-adaptive-history-file             "~/.emacs.d/helm-history")

(define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
(define-key helm-buffer-map (kbd "C-d")     'helm-buffer-run-kill-persistent)

(helm-descbinds-install)                ; integrate w/ describe-bindings
(helm-projectile-on)                    ; integrate w/ projectile
(helm-adaptive-mode t)                  ; use adaptive mode to rank common items
(helm-mode 1)                           ; enable helm!

;;;; COCOA

(defun coda/configure-cocoa ()
  (exec-path-from-shell-initialize)     ; pull PATH from shell environment

  ;; open up maximized-ish
  (let ((px (display-pixel-width))
        (py (display-pixel-height))
        (fx (frame-char-width))
        (fy (frame-char-height))
        tx ty)
    (setq tx (- (/ px fx) 7))
    (setq ty (- (/ py fy) 4))
    (setq initial-frame-alist '((top . 2) (left . 2)))
    (add-to-list 'default-frame-alist (cons 'width tx))
    (add-to-list 'default-frame-alist (cons 'height ty)))

  ;; don't scroll like a maniac
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil)
)
(if (memq window-system '(mac ns)) (coda/configure-cocoa))

;;;; COMPANY

(require 'company)
(define-key company-mode-map [remap hippie-expand] 'company-complete)
(define-key company-active-map [remap hippie-expand] 'company-complete)
(add-hook 'after-init-hook 'global-company-mode)

;;;; SPELLING

(require 'ispell)

(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
          (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.  if we need use
  ;; different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append
;; to the ispell process when "ispell-word" is called.  ispell-extra-args is the
;; command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(add-hook 'text-mode-hook 'flyspell-mode) ; automatically check spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode) ; spell check comments and
                                               ; strings when programming
(add-hook 'git-commit-mode-hook 'flyspell-mode) ; spell check git commit
                                                ; messages

;;;; TERMINAL

(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)

(defun coda/visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term "/usr/local/bin/bash"))
    (switch-to-buffer-other-window "*ansi-term*")))

(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;;;; GO

;; hard-code GOROOT and GOPATH for now
(setenv "GOROOT" "/usr/local/go")
(setenv "GOPATH" "/Users/coda/Projects/go")

(require 'go-mode)
(setq gofmt-command "goimports")        ; use goimports instead of gofmt

(defun coda/configure-go-mode ()
  ;; improve imenu results
  (setq imenu-generic-expression
        '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)
          ("func" "^func *\\(.*\\) {" 1)))
  (imenu-add-to-menubar "Index")

  (add-hook 'before-save-hook 'gofmt-before-save) ; always run gofmt
  (go-eldoc-setup)                                ; use go-eldoc
  (set (make-local-variable 'company-backends) '(company-go))) ; use gocode exclusively

(add-hook 'go-mode-hook 'coda/configure-go-mode)

;;;; RUST

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(require 'racer)
(setq racer-cmd "/Users/coda/Projects/rust/racer/target/release/racer")
(setq racer-rust-src-path "/Users/coda/Projects/rust/rust/src/")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

;;;; JAVASCRIPT

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; KEYBINDINGS

;; M-up and M-down
(move-text-default-bindings)

(global-set-key (kbd "M-;")         'comment-dwim-2)

(global-set-key (kbd "C-c c")       'compile)
(global-set-key (kbd "C-c g")       'magit-status)
(global-set-key (kbd "C-c l p")     'list-packages)
(global-set-key (kbd "C-c r")       'recompile)
(global-set-key (kbd "C-c t")       'coda/visit-term-buffer)

;; helm bindings
(global-set-key (kbd "C-c M-x")     'execute-extended-command) ; old M-x
(global-set-key (kbd "C-x C-d")     'helm-browse-project)
(global-set-key (kbd "C-h C-f")     'helm-apropos)
(global-set-key (kbd "C-h r")       'helm-info-emacs)
(global-set-key (kbd "C-h i")       'helm-info-at-point)
(global-set-key (kbd "C-:")         'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")         'helm-calcul-expression)
(global-set-key (kbd "C-x C-b")     'helm-buffers-list)
(global-set-key (kbd "C-c f")       'helm-recentf)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "M-x")         'helm-M-x)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)
(global-set-key (kbd "C-c i")       'helm-imenu)
(global-set-key (kbd "C-c o")       'helm-swoop)
(global-set-key (kbd "C-x b")       'helm-mini)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "C-c h o")     'helm-occur)

(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)

(global-unset-key (kbd "C-x C-u"))      ; unmap upcase-region, since it always
                                        ; screws with undo

;;;; HASKELL

(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(defun coda-haskell-pointfree-region ()
  "Executes the Haskell pointfree too on the marked region."
  (interactive)
  (let ((pfcmd (format "pointfree %s"
                       (shell-quote-argument (buffer-substring-no-properties
                                              (region-beginning)
                                              (region-end))))))
    (message (format "%s" (shell-command-to-string pfcmd)))))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-tags-on-save t)
  '(haskell-process-type 'cabal-repl))

(require 'haskell-mode)
(require 'haskell-cabal)

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "C-c C-p") 'coda-haskell-pointfree-region)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;;;; CLOJURE

(require 'cider)
(setq cider-repl-use-clojure-font-lock t)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook 'cider-repl-toggle-pretty-printing)
(add-hook 'clojure-mode-hook 'paredit-mode)

;;;; END
