(use-package ag
  :ensure t
  :commands (ag ag-regexp)
  :init
  (setq ag-reuse-buffers 't) ; no need to have a new buffer for every search
  (evil-leader/set-key
    "s" 'ag
    "ös" 'ag-regexp))

(use-package wgrep-ag
  :ensure t
  :defer t)


(provide 'kluge-ag)
