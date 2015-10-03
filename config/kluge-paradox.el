(unless (version< emacs-version "24.4")
  (use-package paradox
    :ensure t
    :defer t
    :config
    (evil-set-initial-state 'paradox-menu-mode 'emacs)))

(provide 'kluge-paradox)
