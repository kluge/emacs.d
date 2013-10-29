(require 'yasnippet)

(yas-global-mode 1)

;; Less verbosity
(setq yas-verbosity 1)

;; Don't enable in ansi-term
(add-hook 'term-mode-hook (lambda ()
			    (yas-minor-mode -1)))

(provide 'kluge-yasnippet)
