(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-indentation-layout-offset 4)
(setq haskell-indentation-left-offset 4)
(setq haskell-indentation-ifte-offset 4)
(setq haskell-indentation-cycle-warn nil) ; don't warn about moving to leftmost position

(provide 'kluge-haskell)
