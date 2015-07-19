(use-package smartparens-config
  :ensure smartparens
  :config	
  (global-set-key (kbd "M-l") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "M-h") 'sp-forward-barf-sexp)
  (global-set-key (kbd "M-L") 'sp-backward-barf-sexp)
  (global-set-key (kbd "M-H") 'sp-backward-slurp-sexp)

  (smartparens-global-mode 1))

(provide 'kluge-smartparens)
