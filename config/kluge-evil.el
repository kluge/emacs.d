(evil-mode 1)   ; enable Evil

;; C-e for end of line in insert state
(define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)

;; M-d for scrolling up in normal state
(define-key evil-normal-state-map (kbd "M-d") 'evil-scroll-up)

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
 "a" 'align-regexp)

;; Surround
(require 'surround)
(global-surround-mode 1)

(provide 'kluge-evil)
