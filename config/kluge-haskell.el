(require 'haskell-mode)

;; Use the newest indentation mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-layout-offset 4)
(setq haskell-indentation-left-offset 4)
(setq haskell-indentation-ifte-offset 4)
(setq haskell-indentation-cycle-warn nil) ; don't warn about moving to leftmost position

;; Scan declarations for imenu
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; Show type signatures in the minibuffer
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)

;; Use ghc-mod
(require 'ghc)
(add-hook 'haskell-mode-hook 'ghc-init)

(provide 'kluge-haskell)
