(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)

  ;; Case-sensitive completion in plain text
  (setq company-dabbrev-downcase nil)

  ;; Move show location to C-l
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-l") 'company-show-location)
  (global-company-mode 1))

(provide 'kluge-company)
