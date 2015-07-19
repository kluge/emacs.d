(defun kluge-set-flycheck-clang-std ()
  (setq-local flycheck-clang-language-standard "c++14"))

(use-package flycheck
  :ensure t
  :config
  ;; Don't complain about comment structure in elisp files
  ;; (from http://stackoverflow.com/questions/15552349/flycheck-how-to-disable-warnning-while-edit-emacs-lisp-scripts)
  (delete-from-list 'flycheck-checkers 'emacs-lisp-checkdoc)
  (add-hook 'c++-mode-hook 'kluge-set-flycheck-clang-std)
  (global-flycheck-mode))

(provide 'kluge-flycheck)
