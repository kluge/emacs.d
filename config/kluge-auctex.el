(require 'tex-site)

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

(provide 'kluge-auctex)
