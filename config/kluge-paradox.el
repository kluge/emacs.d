(unless (version< emacs-version "24.4")
  (use-package paradox
    :ensure t
    :init
    (evil-set-initial-state 'paradox-menu-mode 'emacs)))

(provide 'kluge-paradox)
