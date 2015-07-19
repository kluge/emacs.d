(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)

  ;; Case-sensitive completion in plain text
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)

  ;; Use company instead of evil's completion
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; Move show location to C-l
  (define-key company-active-map (kbd "C-w") nil)
  (define-key company-active-map (kbd "C-l") 'company-show-location)

  (global-company-mode 1)
  (unless (version< emacs-version "24.4")
    (use-package company-quickhelp
      :ensure t
      :config
      (company-quickhelp-mode 1))))

(provide 'kluge-company)
