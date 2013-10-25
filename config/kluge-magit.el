(require 'magit)

;; Global key to invoke magit
(global-set-key (kbd "C-c m") 'magit-status)

;; j and k for movement
(define-key magit-mode-map (kbd "j") 'magit-goto-next-section)
(define-key magit-mode-map (kbd "k") 'magit-goto-previous-section)
(define-key magit-status-mode-map (kbd "j") 'magit-goto-next-section) ; override jump key
(define-key magit-status-mode-map (kbd "k") 'magit-goto-previous-section)
(define-key magit-branch-manager-mode-map (kbd "k") 'magit-goto-previous-section)
;; k used to be magit-discard-item
(define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
(define-key magit-branch-manager-mode-map (kbd "K") 'magit-discard-item)

(provide 'kluge-magit)
