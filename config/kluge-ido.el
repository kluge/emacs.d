(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous-mode t)
(flx-ido-mode 1)
(ido-vertical-mode 1)

;; Preferred file extensions
(setq ido-file-extensions-order '(".org" ".txt" ".py" ".el" ".hs" ".c" ".cpp" ".h"))

;; Ido mode ex bindings
(define-key evil-ex-map "b " 'ido-switch-buffer)
(define-key evil-ex-map "e " 'ido-find-file)

(provide 'kluge-ido)
