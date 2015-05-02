(use-package tex-site
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (progn 
    ;; Style awareness
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)

    ;; Use reftex
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTex t)

    ;; Use mathmode
    (setq LaTeX-math-abbrev-prefix "§")
    (add-hook 'TeX-mode-hook 'LaTeX-math-mode)

    (add-to-list 'evil-emacs-state-modes 'reftex-select-label-mode)

    ;; Save before compiling without asking
    (setq TeX-save-query nil)
    ;; Compile into PDFs
    (setq TeX-PDF-mode t)
    ;; Use Okular to view
    (setq TeX-view-program-list '(("Okular" "okular %o")))
    (setq TeX-view-program-selection '((output-pdf "Okular")))

    ;; Autocompletion from https://github.com/monsanto/auto-complete-auctex
    ;; (not in use since switch to company)
    (use-package auto-complete-auctex
      :disabled t
      :commands ac-auctex-setup
      :init (add-hook 'LaTeX-mode-hook 'ac-auctex-setup))

    ;; Word wrapping for Latex
    (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

    ;; Snippet for extended subscripts
    (evil-define-key 'insert LaTeX-mode-map
      (kbd "M-s") 'kluge-extended-subscript)

    ;; Shortcut for compilation
    (evil-leader/set-key-for-mode 'latex-mode
      "c" 'kluge-compile-latex)))

(defun kluge-extended-subscript ()
  (interactive)
  (yas-expand-snippet "_{$1}$0"))

(defun kluge-compile-latex ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (TeX-command "LaTeX" 'TeX-master-file nil))

(provide 'kluge-auctex)
