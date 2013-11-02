;; Use C-w for window commands even in Emacs state
(setq evil-want-C-w-in-emacs-state t)

(require 'evil)

(evil-mode 1)   ; enable Evil

;; Red cursor for Emacs state
(setq evil-emacs-state-cursor '("#cc4444" box))

;; Y yanks to the end of the line
(define-key evil-normal-state-map "Y" (kbd "y$"))

;; C-e for end of line in insert state
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
;; C-k for killing the line, I don't remember ever using digraphs
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

;; M-d for scrolling up in motion state
(define-key evil-motion-state-map (kbd "M-d") 'evil-scroll-up)

;; Indent on newline
(define-key evil-insert-state-map (kbd "RET") 'evil-ret-and-indent)

;; Opening file in other window
;;(evil binds to 'ffap-other-window, which is not remapped by ido)
(define-key evil-window-map (kbd "C-f") 'find-file-other-window)

;; Switch to buffer in other window
(define-key evil-window-map (kbd "C-b") 'ido-switch-buffer-other-window)

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "ö")

(evil-leader/set-key
  "a" 'align-regexp
  "w" 'kluge-write-whole-file
  "h" 'kluge-horizontal-split
  "v" 'kluge-vertical-split)

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

;; Surround
(require 'surround)
(global-surround-mode 1)

;; Evil nerd-commenter
(setq evilnc-hotkey-comment-operator "gc")
(require 'evil-nerd-commenter)

;; Evil matchit
(require 'evil-matchit)
(define-key evil-normal-state-map "%" 'evilmi-jump-items)

(provide 'kluge-evil)
