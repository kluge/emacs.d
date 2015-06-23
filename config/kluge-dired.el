(use-package dired+
  :ensure t
  :init
  (setq-default dired-listing-switches "-alh")
  (define-key dired-mode-map (kbd "-") 'dired-up-directory))

(provide 'kluge-dired)
