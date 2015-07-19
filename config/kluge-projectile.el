(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)

  (evil-leader/set-key
    "o" 'projectile-find-other-file)

  (setq projectile-completion-system 'helm)
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-ag
  :ensure t
  :config
  ;; Bind only ag, no grep
  (define-key projectile-command-map (kbd "s") 'helm-ag))

(provide 'kluge-projectile)
