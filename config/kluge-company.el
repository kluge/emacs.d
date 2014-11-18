(use-package company
  :init
  (progn
    (setq company-idle-delay t)
    (setq company-minimum-prefix-length 2)
    (global-company-mode 1))
  :config
  (progn
    ;; Move show location to C-l
    (define-key company-active-map (kbd "C-w") nil)
    (define-key company-active-map (kbd "C-l") 'company-show-location)))

(provide 'kluge-company)
