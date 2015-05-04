(use-package flycheck
  :ensure t
  :config
  ;; Don't complain about comment structure in elisp files
  ;; (from http://stackoverflow.com/questions/15552349/flycheck-how-to-disable-warnning-while-edit-emacs-lisp-scripts)
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (global-flycheck-mode))

(provide 'kluge-flycheck)
