(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  :config
  (when (eq system-type 'windows-nt)
    (setq projectile-enable-caching t))

  (evil-leader/set-key
    "o" 'projectile-find-other-file)

  (define-key projectile-command-map (kbd "s") 'projectile-ag)

  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(provide 'kluge-projectile)
