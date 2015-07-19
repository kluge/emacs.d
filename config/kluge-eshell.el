(evil-set-initial-state 'eshell-mode 'emacs)

(add-hook 'eshell-mode-hook 'kluge-eshell-bindings)

(defun kluge-eshell-bindings ()
  (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)
  (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input))

(provide 'kluge-eshell)
