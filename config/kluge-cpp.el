(use-package cc-mode
  :config
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (add-hook 'c++-mode-hook 'kluge-set-cpp-indent))

(defun kluge-set-cpp-indent ()
  (c-set-offset 'innamespace [0]))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\'"))

(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; To avoid delays on windows (while this isn't actually tested on windows...)
  (setq w32-pipe-read-delay 0))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony)
  (setq company-backends (remq 'company-clang company-backends))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package flycheck-irony
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(add-hook 'irony-mode-hook (lambda () (message "irony-mode-hook run!")))

(eval-after-load 'kluge-smartparens
  '(sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(provide 'kluge-cpp)
