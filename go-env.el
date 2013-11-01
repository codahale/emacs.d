(add-to-list 'load-path "/usr/local/Cellar/go/1.2rc3/libexec/misc/emacs/")
(require 'go-mode-load)

; Format Go code every time it's saved.
(add-hook 'before-save-hook 'gofmt-before-save)

; enable Flymake for Go
(add-to-list 'load-path "~/Projects/go/src/github.com/dougm/goflymake")
(require 'go-flymake)
