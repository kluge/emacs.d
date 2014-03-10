(use-package projectile
  :init
  (progn
    ;; Shorter keys
    (define-key evil-normal-state-map (kbd "M-p f") 'projectile-find-file)
    (define-key evil-normal-state-map (kbd "M-p a") 'projectile-ack)
    (define-key evil-normal-state-map (kbd "M-p b") 'projectile-switch-to-buffer))
  :idle
  (projectile-global-mode))

(provide 'kluge-projectile)
