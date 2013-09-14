(require 'ace-jump-mode)

;; Keybindings for Evil integration
 
(define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
(define-key evil-operator-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
(define-key evil-operator-state-map (kbd "C-SPC") 'evil-ace-jump-char-to-mode)

(provide 'kluge-ace-jump)
