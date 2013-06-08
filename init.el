;; Disable extraneous GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Set font size
(set-face-attribute 'default nil :height 85)

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
    auto-complete
    autopair
    epc
    evil
    evil-leader
    flycheck
    ghc
    haskell-mode
    ido
    jedi
    rainbow-mode
    scala-mode2))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load config from separate files
(add-to-list 'load-path "~/.emacs.d/config/")

;; General settings
(require 'kluge-settings)

;; Evil (Extensible Vi Layer)
(require 'kluge-evil)

;; Acejump
(require 'kluge-ace-jump)

;; Auto Complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(ac-config-default)
(global-auto-complete-mode t)

;; Autopair
(require 'autopair)
(autopair-global-mode)
(setq autopair-blink nil) ; don't blink around

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

;; Scala
(require 'kluge-scala)
