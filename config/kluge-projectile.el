(use-package projectile
  :ensure t
  :init
  (setq projectile-keymap-prefix (kbd "M-p"))
  :config
  (setq projectile-enable-caching t)
  ;; Bind only ag, no grep
  (define-key projectile-command-map (kbd "s") 'projectile-ag)

  (evil-leader/set-key
    "o" 'projectile-find-other-file)

  ;; Use ivy
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(provide 'kluge-projectile)
