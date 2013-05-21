(evil-mode 1)   ; enable Evil

;; C-e for end of line in insert state
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)

(provide 'kluge-evil)
