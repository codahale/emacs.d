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
(defadvice company-go (around fix-company-go-prefix activate)
      "Clobber company-go to use company-grab-word instead of the
flakey regular expression. This allows us to complete standard
variables etc. as well as methods and properties."
      ad-do-it
      (when (eql (ad-get-arg 0) 'prefix)
        (setq ad-return-value (company-grab-word))))
))

(provide 'init-go)
