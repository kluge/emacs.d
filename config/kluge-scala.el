(require 'scala-mode2)
(require 'ensime)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (setq evil-shift-width 2)))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(eval-after-load 'kluge-smartparens
  '(sp-local-pair 'scala-mode "{" nil :post-handlers '((kluge-block-after-newline "RET"))))

(defun kluge-block-after-newline (&rest _ignored)
  "Indent the trailing } on a separate line after the cursor."
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(provide 'kluge-scala)
