(use-package multiple-cursors
  :ensure t
  :commands
  (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this-dwim)
  :init
  ;; Keybindings
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C--") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-ö") 'mc/mark-all-like-this-dwim)

  ;; Functions to change to Emacs state and back around multiple-cursors
  ;; invocations, since multiple-cursors does not support Evil
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'kluge-mc-restore-normal-state))

(defun kluge-mc-restore-normal-state ()
  "Restore evil normal state after multiple-cursors."
  (setq mark-active nil) ; multiple-cursors may leave region active
  (evil-force-normal-state))

(provide 'kluge-multiple-cursors)
