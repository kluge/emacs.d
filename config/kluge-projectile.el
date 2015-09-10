(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  :config
  (when (eq system-type 'windows-nt)
    (setq projectile-enable-caching t))

  (evil-leader/set-key
    "o" 'projectile-find-other-file)

  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(use-package helm-ag
  :ensure t
  :config
  ;; Bind only ag, no grep
  (define-key projectile-command-map (kbd "s") 'helm-ag))

(provide 'kluge-projectile)
