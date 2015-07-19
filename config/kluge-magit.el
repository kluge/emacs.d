(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")

  ;; Global key to invoke magit
  (global-set-key (kbd "C-c m") 'magit-status)

  ;; Leader keys
  (evil-leader/set-key
    "g s" 'magit-status
    "g b" 'magit-blame
    "g l" 'magit-log-buffer-file
    "g d" 'vc-diff)

  ;; Reuse windows for magit-status
  (setq magit-status-buffer-switch-function 'switch-to-buffer)

  :config
  ;; j and k for movement
  (dolist (map (list magit-mode-map magit-status-mode-map magit-diff-mode-map))
    (define-key map (kbd "j") 'magit-section-forward))
  (dolist (map (list magit-mode-map
                     magit-untracked-section-map
                     magit-branch-section-map
                     magit-remote-section-map
                     magit-tag-section-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-unstaged-section-map
                     magit-staged-section-map
                     magit-stashes-section-map
                     magit-stash-section-map))
    (define-key map (kbd "k") 'magit-section-backward))
  ;; Rebind discarding commands to K
  (define-key magit-refs-mode-map (kbd "K") 'magit-remote-remove)
  (dolist (map (list magit-untracked-section-map
                     magit-file-section-map
                     magit-hunk-section-map
                     magit-unstaged-section-map
                     magit-staged-section-map))
    (define-key map (kbd "K") 'magit-discard))
  (define-key magit-branch-section-map (kbd "K") 'magit-branch-delete)
  (define-key magit-remote-section-map (kbd "K") 'magit-remote-remove)
  (define-key magit-tag-section-map (kbd "K") 'magit-tag-delete)
  (define-key magit-stashes-section-map (kbd "K") 'magit-stash-clear)
  (define-key magit-stash-section-map (kbd "K") 'magit-stash-drop)

  ;; Blame mode
  (define-key magit-blame-mode-map (kbd "j") 'magit-blame-next-chunk)
  (define-key magit-blame-mode-map (kbd "k") 'magit-blame-previous-chunk)
  (add-hook 'magit-blame-mode-hook (lambda ()
                                     (evil-emacs-state)))
  (advice-add 'magit-blame-quit :after 'evil-normal-state)

  ;; Automatically revert asynchronously
  (setq magit-revert-buffers 0.5)

  ;; Start commit message window in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state))

(use-package git-rebase
  :config
  (define-key git-rebase-mode-map (kbd "j") 'forward-line)
  (define-key git-rebase-mode-map (kbd "k") 'git-rebase-backward-line)
  (define-key git-rebase-mode-map (kbd "K") 'git-rebase-kill-line))

(provide 'kluge-magit)
