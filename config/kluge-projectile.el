(use-package projectile
  :ensure t
  :config
  ;; Shorter keys
  (define-key evil-normal-state-map (kbd "M-p f") 'projectile-find-file)
  (define-key evil-normal-state-map (kbd "M-p s") 'projectile-ag)
  (define-key evil-normal-state-map (kbd "M-p b") 'projectile-switch-to-buffer)

  ;; Use ivy
  (setq projectile-completion-system 'ivy)
  (projectile-global-mode))

(provide 'kluge-projectile)
