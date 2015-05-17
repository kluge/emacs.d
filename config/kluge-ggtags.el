
(use-package ggtags
  :ensure t
  :config
  (evil-leader/set-key
    "r" 'ggtags-find-reference)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		(ggtags-mode 1)))))

(provide 'kluge-ggtags)
