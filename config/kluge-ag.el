(use-package ag
  :ensure t
  :init
  (setq ag-reuse-buffers 't) ; no need to have a new buffer for every search
  (evil-leader/set-key
    "s" 'ag
    "ös" 'ag-regexp))


(provide 'kluge-ag)