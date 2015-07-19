(use-package ggtags
  :ensure t
  :defer t)

(use-package helm-gtags
  :ensure t
  :defer t
  :init
  (setq helm-gtags-auto-update t)
  (setq helm-gtags-pulse-at-cursor nil)
  :config
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-w .") 'helm-gtags-find-tag-other-window)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (evil-leader/set-key
    "r" 'helm-gtags-find-rtag))

(provide 'kluge-gtags)
