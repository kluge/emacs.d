(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'asm-mode)

(setq ac-ignore-case nil) ; don't ignore case

(provide 'kluge-auto-complete)
