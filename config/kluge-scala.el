(require 'scala-mode2)

(add-hook 'scala-mode-hook
	  '(lambda ()
	     (setq evil-shift-width 2)))


(provide 'kluge-scala)
