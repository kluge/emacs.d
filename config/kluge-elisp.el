;; Set Evil indent step to 2
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq evil-shift-width 2)))

(defvar kluge-elisp-mode-hooks '(emacs-lisp-mode-hook
                                 lisp-interaction-mode-hook
                                 ielm-mode-hook))

(use-package eldoc
  :diminish eldoc-mode
  :config
  ;; Enable eldoc (show function arguments in the minibuffer)
  (dolist (hook kluge-elisp-mode-hooks)
    (add-hook 'hook 'turn-on-eldoc-mode)))

(dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (evil-leader/set-key-for-mode mode
    "e" 'eval-last-sexp
    "E" 'eval-defun))

(use-package elisp-slime-nav
  :ensure t
  :init
  (dolist (hook kluge-elisp-mode-hooks)
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(provide 'kluge-elisp)
