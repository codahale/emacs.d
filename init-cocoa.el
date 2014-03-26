(when (memq window-system '(mac ns))
  (progn
    (exec-path-from-shell-initialize) ; load path from shell
    (add-to-list 'default-frame-alist '(width . 200)) ; open up to 200x60
    (add-to-list 'default-frame-alist '(height . 60))
    (setq mouse-wheel-scroll-amount '(1)) ; stop scrolling so damn fast
    (setq mouse-wheel-progressive-speed nil)
    (setenv "GOROOT" "/usr/local/go") ; use the pkg-installed GOROOT
    (setenv "GOPATH" "/Users/coda/Projects/go") ; ok no for real
))

(provide 'init-cocoa)
