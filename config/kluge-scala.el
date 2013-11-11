(require 'scala-mode2)
(require 'ensime)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (setq evil-shift-width 2)))

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'kluge-scala)
