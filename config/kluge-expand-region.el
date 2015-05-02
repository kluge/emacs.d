(use-package expand-region
  :ensure t
  :init
  (progn 
    ;; Efficient bindings stolen from http://blog.binchen.org/?p=782
    (evil-leader/set-key
      "x" 'er/expand-region)
    (setq expand-region-contract-fast-key "z")))

(provide 'kluge-expand-region)
