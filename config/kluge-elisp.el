;; Set Evil indent step to 2
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq evil-shift-width 2)))

(provide 'kluge-elisp)
