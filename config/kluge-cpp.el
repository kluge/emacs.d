(use-package cc-mode
  :mode ("\\.h\\'" . c++-mode)
  :config
  (setq c-default-style "bsd")
  (setq-default c-basic-offset 4)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'kluge-set-cpp-indent))

(defun kluge-set-cpp-indent ()
  (c-set-offset 'innamespace [0]))

(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\'")
  :init
  (add-hook 'cmake-mode-hook 'kluge-init-cmake-mode))

(defun kluge-init-cmake-mode ()
  (add-to-list 'company-backends 'company-cmake))

(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'kluge-init-company-c-headers))

(defun kluge-init-company-c-headers ()
  (add-to-list 'company-backends 'company-c-headers))

(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; To avoid delays on windows (though this isn't actually tested on windows...)
  (setq w32-pipe-read-delay 0))

(use-package company-irony
  :ensure t
  :commands (company-irony company-irony-setup-begin-commands)
  :init
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  :config
  (add-to-list 'company-backends 'company-irony)
  (delete-from-list 'company-backends 'company-clang))

(use-package flycheck-irony
  :ensure t
  :commands (flycheck-irony-setup)
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package irony-eldoc
  :ensure t
  :commands (irony-eldoc)
  :init
  (add-hook 'irony-mode-hook 'irony-eldoc))

(eval-after-load 'kluge-smartparens
  '(sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package gdb-mi
  :defer t
  :init
  (setq gdb-show-main t)
  :config
  (evil-set-initial-state 'gud-mode 'emacs)
  (add-hook 'gdb-mode-hook 'kluge-disable-gud-gdb-completion))

(defun kluge-disable-gud-gdb-completion ()
  ;; Disable due to blocking completion
  (delete-from-list 'completion-at-point-functions 'gud-gdb-completion-at-point))
         

(provide 'kluge-cpp)
