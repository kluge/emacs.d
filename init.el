;; Disable extraneous GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

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
    auto-complete
    diminish
    epc
    evil
    evil-leader
    evil-indent-textobject
    flycheck
    flx-ido
    ghc
    haskell-mode
    ido
    ido-ubiquitous
    jedi
    projectile
    rainbow-mode
    scala-mode2
    smartparens
    smex
    surround
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load config from separate files
(add-to-list 'load-path "~/.emacs.d/config/")

;; Save customizations to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; General settings
(require 'kluge-settings)

;; Evil (Extensible Vi Layer)
(require 'kluge-evil)
(require 'evil-indent-textobject)

;; Acejump
(require 'kluge-ace-jump)

;; Ack and a half
(require 'kluge-ack-and-a-half)

;; Auto Complete
(require 'kluge-auto-complete)

;; Flycheck
(require 'kluge-flycheck)

;; Haskell
(require 'kluge-haskell)

;; Ido
(require 'kluge-ido)

;; Jedi
(autoload 'jedi:ac-setup "jedi" nil t)
; Provide auto completion in python-mode
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Org
(require 'kluge-org)

;; Projectile
(require 'projectile)
(projectile-global-mode)

;; Scala
(require 'kluge-scala)

;; Smartparens
(require 'kluge-smartparens)

;; Smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; Binding for current date
(defun kluge-insert-date (arg)
  (interactive "P")
  (insert (cond
	   ((not arg) (format-time-string "%Y-%m-%d"))
	   ((equal arg '(4)) (format-time-string "%Y-%m-%dT%H:%M"))
	   ((equal arg '(16)) (format-time-string "%Y-%m-%d %H:%M")))))

(define-key evil-insert-state-map (kbd "<f5>") 'kluge-insert-date)

;; Diminish
(require 'diminish)
; Clean up modes that don't need to show on the modeline
(diminish 'undo-tree-mode)
