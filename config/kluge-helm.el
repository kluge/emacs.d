(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (setq helm-display-header-line nil)

  (define-key evil-normal-state-map (kbd "M-y") 'helm-show-kill-ring)

  ;; Fuzzy matching
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-M-x-fuzzy-match t)
  :config
  ;; Smaller source header
  (set-face-attribute 'helm-source-header nil :height 0.5)

  ;; Align helm windows to the frame bottom
  (unless (version< emacs-version "24.1")
    (use-package shackle
      :ensure t
      :init
      (setq shackle-rules
            '(("\\`\\*helm.*?\\*\\'" :regexp t :align 'below)))))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<backtab>") 'helm-select-action)

  (helm-autoresize-mode 1))

(use-package helm-dash
  :if (eq system-type 'gnu/linux)
  :ensure t
  :defer t
  :init
  (evil-leader/set-key
    "d" 'helm-dash))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(provide 'kluge-helm)
