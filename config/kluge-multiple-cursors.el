(use-package multiple-cursors
  :commands
  (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this-dwim)
  :init
  ;; Keybindings
  (global-set-key (kbd "C-+") 'mc/mark-next-like-this)
  (global-set-key (kbd "C--") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-ö") 'mc/mark-all-like-this-dwim)

  ;; Functions to change to Emacs state and back around multiple-cursors
  ;; invocations, since multiple-cursors does not support Evil
  (add-hook 'multiple-cursors-mode-enabled-hook 'kluge-mc-store-state-and-emacs)
  (add-hook 'multiple-cursors-mode-disabled-hook 'kluge-mc-restore-state))

(defvar kluge-mc-old-state nil
  "Stores the state we were in before enabling multiple-cursors.")
(defvar kluge-mc nil
  "Whether we already stored the state for this invocation of
  multiple-cursors.")

(defun kluge-mc-store-state-and-emacs ()
  "Store the state we were in before enabling multiple-cursors
and change to Emacs state."
  (unless kluge-mc
    (setq kluge-mc-old-state evil-state)
    (setq kluge-mc t)
    (evil-emacs-state)))

(defun kluge-mc-restore-state ()
  "Restore the state we were in before enabling multiple-cursors."
  (setq kluge-mc nil)
  (evil-change-state kluge-mc-old-state))

(provide 'kluge-multiple-cursors)
