(require 'org)
(require 'evil)

;; Org file paths
(setq org-agenda-files '("~/org"))
(setq org-default-notes-file "~/org/inbox.org")
(setq org-refile-targets '((nil . (:maxlevel . 3))
			   (org-agenda-files . (:maxlevel . 3))))

;; Use Ido for buffer switching
(setq org-completion-use-ido t)

;; Indent and show less stars
(setq org-startup-indented t)

;; Log date when a task is done
(setq org-log-done 'time)

;; Capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/org/inbox.org")
	 "* TODO %?\n%U\n%i")
	("l" "Todo with link" entry (file "~/org/inbox.org")
	 "* TODO %?\n%U\n%i\n%a")
	("n" "Note" entry (file "~/org/inbox.org")
	 "* %?\n%U\n%i\n")
	("m" "Note with link" entry (file "~/org/inbox.org")
	 "* %?\n%U\n%i\n%a")
	("j" "Journal" entry (file+datetree "~/journal/2013.org")
	 "* %U\n%?")))

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
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)

;; Normal state hotkeys
(evil-define-key 'normal org-mode-map
  (kbd "M-<return>") 'kluge-org-meta-return
  (kbd "M-S-<return>") 'kluge-org-insert-todo-heading
  (kbd "gh") 'outline-up-heading
  (kbd "gj") 'org-forward-heading-same-level
  (kbd "gk") 'org-backward-heading-same-level
  (kbd "gl") 'outline-next-visible-heading
  (kbd "^") 'org-beginning-of-line
  (kbd "$") 'org-end-of-line
  (kbd "<") 'org-metaleft
  (kbd ">") 'org-metaright
  )

;; Normal and insert state hotkeys
(mapc (lambda (state)
	      (evil-define-key state org-mode-map
		(kbd "M-l") 'org-metaright
		(kbd "M-h") 'org-metaleft
		(kbd "M-k") 'org-metaup
		(kbd "M-j") 'org-metadown
		(kbd "M-L") 'org-shiftmetaright
		(kbd "M-H") 'org-shiftmetaleft
		(kbd "M-K") 'org-shiftmetaup
		(kbd "M-J") 'org-shiftmetadown))
      '(normal insert))

(provide 'kluge-org)
