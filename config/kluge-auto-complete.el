(use-package auto-complete-config
  :disabled t
  :ensure auto-complete
  :init
  (progn 
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
    (ac-config-default)
    (global-auto-complete-mode t)
    (add-to-list 'ac-modes 'asm-mode)
    ;; don't ignore case
    (setq ac-ignore-case nil)))

(provide 'kluge-auto-complete)
