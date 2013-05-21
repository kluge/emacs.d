
;; Put backup files in a special directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
;; Put all auto-save files to /tmp
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Visual settings
(show-paren-mode 1)	; highlight matching parenthesis
(global-hl-line-mode 1) ; highlight current line
(line-number-mode 1)    ; show line number in modeline
(column-number-mode 1)	; show column number in modeline
(global-linum-mode -1)  ; don't show line numbers

;; Code settings
(setq indent-tabs-mode nil) ; use spaces to indent

(provide 'kluge-settings)
