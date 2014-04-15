(use-package company
  :init
  (progn
    (setq company-idle-delay t)
    (setq company-minimum-prefix-length 2)
    (global-company-mode 1)))

(provide 'kluge-company)
