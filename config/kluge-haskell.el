(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :mode "\\.l?hs$"
  :init
  (setq evil-auto-indent nil) ; disable evil's indenting

  ;; Scan declarations for imenu
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

  ;; Show type signatures in the minibuffer
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
  ;; But not for core language
  (setq haskell-doc-show-reserved nil)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (setq haskell-interactive-popup-errors nil)
  (use-package haskell-interactive-mode
    :init
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
    (evil-set-initial-state 'haskell-interactive-mode 'emacs)
    (setq haskell-interactive-popup-errors nil)
    :config
    (define-key haskell-interactive-mode-map (kbd "C-p") 'haskell-interactive-mode-history-previous)
    (define-key haskell-interactive-mode-map (kbd "C-n") 'haskell-interactive-mode-history-next))

  (setq haskell-process-type 'stack-ghci))

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-configure
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))

(use-package shakespeare-mode
  :ensure t)


(provide 'kluge-haskell)
