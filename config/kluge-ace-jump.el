(use-package ace-jump-mode
  :ensure t
  :commands
  (evil-ace-jump-char-mode evil-ace-jump-char-to-mode evil-ace-jump-line-mode)
  :init
  (progn ;; Keybindings for Evil integration
    (define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
    (define-key evil-motion-state-map (kbd "C-SPC") 'evil-ace-jump-char-to-mode)
    (define-key evil-motion-state-map (kbd "g SPC") 'evil-ace-jump-line-mode)))

(provide 'kluge-ace-jump)
