(require 'ido)
(require 'ido-ubiquitous)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode t)

;; Preferred file extensions
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".el" ".hs"))

;; Ido mode ex bindings
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "e " 'ido-find-file)

(provide 'kluge-ido)
