(use-package ido
  :ensure t
  :init
  (progn 
    (use-package ido-ubiquitous
      :ensure t)
    (use-package flx-ido
      :ensure t)

    ;; Ido everywhere with better flex matching
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)
    (ido-ubiquitous-mode t)
    (flx-ido-mode 1)
    (ido-vertical-mode 1)

    ;; Don't ignore case
    (setq ido-case-fold nil)

    ;; Preferred file extensions
    (setq ido-file-extensions-order '(".org" ".txt" ".py" ".el" ".hs" ".c" ".cpp" ".h" ".tex"))

    ;; Ido mode ex bindings
    (define-key evil-ex-map "b " 'ido-switch-buffer)
    (define-key evil-ex-map "e " 'ido-find-file)

    ;; And leader bindings
    (evil-leader/set-key
      "b" 'ido-switch-buffer
      "f" 'ido-find-file)))

(provide 'kluge-ido)
