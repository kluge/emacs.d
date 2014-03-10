(use-package ack-and-a-half
  :commands
  (ack ack-same ack-find-file ack-find-file-same)
  :init
  (progn  
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

    ;; Keybindings for moving between matches
    (define-key evil-normal-state-map (kbd "M-j") 'next-error)
    (define-key evil-normal-state-map (kbd "M-k") 'previous-error)

    (evil-leader/set-key
      "s" 'ack
      "ös" 'ack-same)))

(provide 'kluge-ack-and-a-half)
