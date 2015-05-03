;; Use C-w for window commands even in Emacs state
(setq evil-want-C-w-in-emacs-state t)

;; Use gc as the comment operator
(setq evilnc-hotkey-comment-operator "gc")

(use-package evil
  :ensure t
  :init
  ;; Evil leader
  (use-package evil-leader
    :ensure t
    :config
    (progn
      (global-evil-leader-mode)
      (evil-leader/set-leader "ö")

      (evil-leader/set-key
	"a" 'align-regexp
	"i" 'imenu
	"w" 'kluge-write-whole-file
	"W" 'evil-write-all
	"h" 'kluge-horizontal-split
	"v" 'kluge-vertical-split)))

  :config
  (evil-mode 1)   ; enable Evil

  ;; Red cursor for Emacs state
  (setq evil-emacs-state-cursor '("#cc4444" box))
  (setq evil-normal-state-cursor '("#00ff00" box))
  (setq evil-insert-state-cursor '("#00ff00" (bar . 2)))

  ;; Y yanks to the end of the line
  (define-key evil-normal-state-map "Y" (kbd "y$"))

  ;; C-e for end of line in insert state
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  ;; C-k for killing the line, I don't remember ever using digraphs
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  ;; M-d for scrolling up, C-d for scrolling down
  (global-set-key (kbd "M-d") 'evil-scroll-up)
  (global-set-key (kbd "C-d") 'evil-scroll-down)

  ;; Indent on newline
  (define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

  ;; Keybindings for moving between matches in search results and such
  (define-key evil-normal-state-map (kbd "M-j") 'next-error)
  (define-key evil-normal-state-map (kbd "M-k") 'previous-error)

  ;; Opening file in other window
  ;;(evil binds to 'ffap-other-window, which is not remapped by ido)
  (define-key evil-window-map (kbd "C-f") 'find-file-other-window)

  ;; Switch to buffer in other window
  (define-key evil-window-map (kbd "C-b") 'ido-switch-buffer-other-window)

  ;; Don't override M-., it is useful. Rotate the repeat ring with C-, instead.
  (define-key evil-normal-state-map (kbd "M-.") nil)
  (define-key evil-normal-state-map (kbd "C-,") 'evil-repeat-pop-next)

  ;; Restore visual selection after shifting
  (define-key evil-visual-state-map (kbd "<") 'kluge-shift-left-and-restore-visual)
  (define-key evil-visual-state-map (kbd ">") 'kluge-shift-right-and-restore-visual)

  ;; Normal state ! for filtering text with shell commands
  ;; from https://gist.github.com/cofi/7589306
  (defun evil-filter (count)
    (interactive "p")
    (evil-ex (if current-prefix-arg
		 (format ".,+%d!" count)
	       ".!")))
  
  (define-key evil-normal-state-map (kbd "!") #'evil-filter)

  ;; Surround
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  ;; Evil nerd-commenter
  (use-package evil-nerd-commenter
    :ensure t)

  ;; Evil matchit
  (use-package evil-matchit
    :ensure t
    :config
    (progn
      (defun evilmi-customize-keybinding ()
	(define-key evil-normal-state-map "%" 'evilmi-jump-items))
      (global-evil-matchit-mode 1)))

  ;; Evil jumper (jumps between buffers)
  (use-package evil-jumper
    :ensure t
    :config
    (global-evil-jumper-mode 1)))

(defun kluge-write-whole-file ()
  (interactive)
  (evil-write nil nil))

(defun kluge-vertical-split ()
  (interactive)
  (split-window-right)
  (evil-window-right 1))

(defun kluge-horizontal-split ()
  (interactive)
  (split-window-below)
  (evil-window-down 1))

(evil-define-operator kluge-shift-left-and-restore-visual (beg end &optional count)
  :type line
  (interactive "<r><vc>")
  (evil-shift-left beg end (or count 1))
  (evil-normal-state) ; needed for some reason?
  (evil-visual-restore))

(evil-define-operator kluge-shift-right-and-restore-visual (beg end &optional count)
  :type line
  (interactive "<r><vc>")
  (evil-shift-right beg end (or count 1))
  (evil-normal-state) ; needed for some reason?
  (evil-visual-restore))

(provide 'kluge-evil)
