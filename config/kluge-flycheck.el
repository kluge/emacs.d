(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Don't complain about comment structure in elisp files
;; (from http://stackoverflow.com/questions/15552349/flycheck-how-to-disable-warnning-while-edit-emacs-lisp-scripts)
(eval-after-load 'flycheck
  '(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers)))

(provide 'kluge-flycheck)
