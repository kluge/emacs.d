(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'c++-mode-hook 'aggressive-indent-mode))

(provide 'kluge-aggressive-indent)
