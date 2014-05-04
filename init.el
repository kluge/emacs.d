(defconst emacs-start-time (current-time))

(require 'benchmark-init nil t)

;; Disable extraneous GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Set frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))

;; Set font size
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :height 100)
  (set-face-attribute 'default nil :height 85))

;; Show current buffer in frame title
(setq frame-title-format '("" "%b - Emacs"))

;; Use Peakburn color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'peakburn t)

;; Set up the package system
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install the packages, if they're not already installed
(defvar my-packages
  '(ace-jump-mode
    ack-and-a-half
    auctex
    benchmark-init
    company
    diminish
    ensime
    epc
    evil
    evil-indent-textobject
    evil-leader
    evil-matchit
    evil-nerd-commenter
    expand-region
    flx-ido
    flycheck
    ghc
    google-this
    goto-chg
    haskell-mode
    ido
    ido-ubiquitous
    ido-vertical-mode
    magit
    multiple-cursors
    org
    projectile
    rainbow-mode
    scala-mode2
    smart-mode-line
    smartparens
    smex
    surround
    use-package
    yasnippet
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load config from separate files
(add-to-list 'load-path "~/.emacs.d/config/")

;; Unpackaged elisp
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Save customizations to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use-package
(require 'use-package)

;; General settings
(require 'kluge-settings)

;; Evil (Extensible Vi Layer)
(require 'kluge-evil)
(use-package evil-indent-textobject)

;; Acejump
(require 'kluge-ace-jump)

;; Ack and a half
(require 'kluge-ack-and-a-half)

;; Ansi-term
(require 'kluge-ansi-term)

;; Auctex
(require 'kluge-auctex)

;; Benchmark-init-modes
(use-package benchmark-init-modes
  :commands (benchmark-init/show-durations-tabulated))

;; YASnippet
(require 'kluge-yasnippet)

;; Company
(require 'kluge-company)

;; Emacs Lisp
(require 'kluge-elisp)

;; Expand region
(require 'kluge-expand-region)

;; Flycheck
(require 'kluge-flycheck)

;; Google this
(require 'kluge-google-this)

;; Haskell
(require 'kluge-haskell)

;; Ido
(require 'kluge-ido)

;; Org
(require 'kluge-org)

;; Projectile
(require 'kluge-projectile)

;; Magit
(require 'kluge-magit)

;; Mu4e
(require 'kluge-mu4e)

;; Multiple cursors
(require 'kluge-multiple-cursors)

;; Scala
(require 'kluge-scala)

;; Smart-mode-line
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; Smartparens
(require 'kluge-smartparens)

;; Smex
(use-package smex
  :init
  (progn
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

;; Uniquify
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse))

;; Winner-mode
(winner-mode 1)

;; Binding for current date
(defun kluge-insert-date (arg)
  (interactive "P")
  (insert (cond
	   ((not arg) (format-time-string "%Y-%m-%d"))
	   ((equal arg '(4)) (format-time-string "%Y-%m-%dT%H:%M"))
	   ((equal arg '(16)) (format-time-string "%Y-%m-%d %H:%M")))))

(define-key evil-insert-state-map (kbd "<f5>") 'kluge-insert-date)

;; Diminish
(use-package diminish
  :init
  (progn
    ;; Clean up modes that don't need to show on the modeline
    (diminish 'projectile-mode)
    (diminish 'undo-tree-mode)))


;; Emacs startup time measuring code from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))


(add-hook 'after-init-hook
	  `(lambda ()
	     (let ((elapsed (float-time (time-subtract (current-time)
						       emacs-start-time))))
	       (message "Loading %s...done (%.3fs) [after-init]"
			,load-file-name elapsed)))
	  t)

