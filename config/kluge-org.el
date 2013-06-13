(require 'org)

;; Org file paths
(setq org-agenda-files '("~/org"))
(setq org-default-notes-file "~/org/inbox.org")

;; Use Ido for buffer switching
(setq org-completion-use-ido t)

;; Global hotkeys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c r") 'org-capture)


(provide 'kluge-org)
