(require 'org)

;; Org file paths
(setq org-agenda-files '("~/org"))
(setq org-default-notes-file "~/org/inbox.org")
(setq org-refile-targets '((nil . (:maxlevel . 3))
			   (org-agenda-files . (:maxlevel . 3))))

;; Use Ido for buffer switching
(setq org-completion-use-ido t)

;; Commands
(evil-define-command kluge-org-meta-return (&optional count argument)
  "org-meta-return and insert state"
  :repeat t
  (end-of-line)
  (org-meta-return)
  (evil-insert 1))

(evil-define-command kluge-org-insert-todo-heading (&optional count argument)
  "org-meta-return and insert state"
  :repeat t
  (end-of-line)
  (org-insert-todo-heading argument)
  (evil-insert 1))

;; Global hotkeys
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c r") 'org-capture)

;; Mode hotkeys
(evil-define-key 'normal org-mode-map
  (kbd "M-<return>") 'kluge-org-meta-return
  (kbd "M-S-<return>") 'kluge-org-insert-todo-heading)

(provide 'kluge-org)
