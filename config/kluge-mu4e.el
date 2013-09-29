;; Basic options
(when (require 'mu4e nil t)
  (setq mu4e-maildir "/home/kluge/maildir-kapsi")
  (setq mu4e-attachment-dir "~/incoming")

  (setq mu4e-sent-folder   "/sent"
	mu4e-drafts-folder "/drafts"
	mu4e-trash-folder  "/trash"
	mu4e-refile-folder "/archive")

  ;; Don't run a mail command, just update index
  (setq mu4e-get-mail-command "true")
  ;; Update the index every 5 minutes
  (setq mu4e-update-interval 300)

  (setq message-send-mail-function   'smtpmail-send-it
	smtpmail-smtp-server         "mail.kapsi.fi"
	smtpmail-stream-type         'starttls
	smtpmail-smtp-service        587)

  ;; Private options
  (add-hook 'mu4e-main-mode-hook (lambda ()
				   (load-library "kluge-mu4e-private.el.gpg")))

  ;; UI options
  (setq mu4e-view-show-addresses t)

  ;; Don't keep sent message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Emacs state in mu4e modes
  (evil-set-initial-state 'mu4e-main-mode 'emacs)
  (evil-set-initial-state 'mu4e-headers-mode 'emacs)
  (evil-set-initial-state 'mu4e-view-mode 'emacs)

  ;; Use f for going to a folder
  (define-key mu4e-main-mode-map (kbd "f") 'mu4e~headers-jump-to-maildir)
  (define-key mu4e-headers-mode-map (kbd "f") 'mu4e~headers-jump-to-maildir)
  (define-key mu4e-view-mode-map (kbd "f") 'mu4e~headers-jump-to-maildir)

  ;; Use j and k to move in messages
  (define-key mu4e-headers-mode-map (kbd "j") 'mu4e-headers-next)
  (define-key mu4e-headers-mode-map (kbd "k") 'mu4e-headers-prev)
  (define-key mu4e-view-mode-map (kbd "j") 'mu4e-view-headers-next)
  (define-key mu4e-view-mode-map (kbd "k") 'mu4e-view-headers-prev)

  ;; Use C-d and M-d to scroll down and up
  (define-key mu4e-headers-mode-map (kbd "C-d") 'evil-scroll-down)
  (define-key mu4e-headers-mode-map (kbd "M-d") 'evil-scroll-up)
  (define-key mu4e-view-mode-map (kbd "C-d") 'evil-scroll-down)
  (define-key mu4e-view-mode-map (kbd "M-d") 'evil-scroll-up)

  ;; Canning spam
  (defun kluge-mu4e-headers-spam ()
    "Move the current message to spam folder."
    (interactive)
    (mu4e-mark-set 'move "/spam")
    (mu4e-headers-next))
  (defun kluge-mu4e-view-spam ()
    "Move the current message to spam folder."
    (interactive)
    (mu4e~view-in-headers-context
     (mu4e-mark-set 'move "/spam"))
    (mu4e-view-headers-next))

  (define-key mu4e-headers-mode-map (kbd "c") 'kluge-mu4e-headers-spam)
  (define-key mu4e-view-mode-map (kbd "c") 'kluge-mu4e-view-spam))

(provide 'kluge-mu4e)
