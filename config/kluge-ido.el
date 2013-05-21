(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Ido mode ex bindings
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "e " 'ido-find-file)

(provide 'kluge-ido)
