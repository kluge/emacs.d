;; Set Evil indent step to 2
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq evil-shift-width 2)))

;; Enable eldoc (show function arguments in the minibuffer)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(evil-leader/set-key
  "e" 'eval-last-sexp
  "E" 'eval-defun)

(provide 'kluge-elisp)
