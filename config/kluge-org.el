(use-package org
  :ensure t
  :defer 5
  :init
  ;; Org file paths
  (setq org-agenda-files '("~/org"))
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-refile-targets '((nil . (:maxlevel . 3))
			     (org-agenda-files . (:maxlevel . 1))))

  ;; Use Ido for buffer switching
  (setq org-completion-use-ido t)

  ;; Indent and show less stars
  (setq org-startup-indented t)

  ;; Log date when task state is changed
  (setq org-todo-keywords
	'((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (setq org-log-into-drawer t)

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
	  ("j" "Journal" entry (file+datetree "~/journal/2014.org")
	   "* %U\n%?")))

  ;; Habit tracking
  (setq org-habit-following-days 0) ; don't show future days
  (setq org-habit-show-done-always-green t) ; show done on a green background

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

  (evil-leader/set-key-for-mode 'org-mode
    "t" 'org-todo)

  ;; Normal and insert state hotkeys
  (evil-define-key 'normal org-mode-map
    (kbd "M-l") 'org-metaright
    (kbd "M-h") 'org-metaleft
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "M-L") 'org-shiftmetaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-K") 'org-shiftmetaup
    (kbd "M-J") 'org-shiftmetadown)
  (evil-define-key 'insert org-mode-map
    (kbd "M-l") 'org-metaright
    (kbd "M-h") 'org-metaleft
    (kbd "M-k") 'org-metaup
    (kbd "M-j") 'org-metadown
    (kbd "M-L") 'org-shiftmetaright
    (kbd "M-H") 'org-shiftmetaleft
    (kbd "M-K") 'org-shiftmetaup
    (kbd "M-J") 'org-shiftmetadown)

  (eval-after-load "org-agenda"
    '(progn
       ;; j and k for movement in agenda
       (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
       (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
       (define-key org-agenda-mode-map (kbd "J") 'org-agenda-goto-date)))

  ;; Don't confirm following vlc links
  (setq org-confirm-shell-link-not-regexp "vlc")

  :config
  ;; Start in insert state in capture mode
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  ;; Go to Emacs state for org-goto
  ;; (gotten from http://emacs.stackexchange.com/questions/883/using-evil-mode-with-a-function-that-does-not-work-well-with-evil-mode)
  (defadvice org-goto (around org-goto-evil-emacs-state activate)
    (let ((orig-state evil-state)
  	  (evil-emacs-state-modes (cons 'org-mode evil-emacs-state-modes)))
      ad-do-it
      (evil-change-state orig-state))))

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

(provide 'kluge-org)
