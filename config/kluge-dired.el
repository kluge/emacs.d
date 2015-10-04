(use-package dired+
  :ensure t
  :config
  (setq-default dired-listing-switches "-alh")
  (setq dired-dwim-target t) ; Suggest targets from the other open dired buffer
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(provide 'kluge-dired)
