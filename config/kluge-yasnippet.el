(use-package yasnippet
  :ensure t
  :defer 5
  :config
  ;; Don't use GTK dropdowns
  (delete-from-list 'yas-prompt-functions 'yas-x-prompt)
  ;; Only use my own snippets
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  ;; Less verbosity
  (setq yas-verbosity 1)
  (yas-global-mode 1))

(provide 'kluge-yasnippet)
