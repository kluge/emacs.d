(use-package smartparens-config
  :ensure smartparens
  :init	
  (progn
    (smartparens-global-mode t)
    (define-key evil-normal-state-map (kbd "M-l") 'sp-forward-slurp-sexp)
    (define-key evil-normal-state-map (kbd "M-h") 'sp-forward-barf-sexp)
    (define-key evil-normal-state-map (kbd "M-L") 'sp-backward-barf-sexp)
    (define-key evil-normal-state-map (kbd "M-H") 'sp-backward-slurp-sexp)))

(provide 'kluge-smartparens)
