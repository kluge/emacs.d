(use-package cc-mode
  :config
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\'"))

(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(provide 'kluge-cpp)
