(defun kluge-set-flycheck-clang-std ()
	      (setq-local flycheck-clang-language-standard "c++11"))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  ;; Don't complain about comment structure in elisp files
  ;; (from http://stackoverflow.com/questions/15552349/flycheck-how-to-disable-warnning-while-edit-emacs-lisp-scripts)
  (setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
  (add-hook 'c++-mode-hook 'kluge-set-flycheck-clang-std))


(provide 'kluge-flycheck)
