
;; Disable extraneous GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Set font size
(set-face-attribute 'default nil :height 85)

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
  '(evil
    ghc
    haskell-mode
    rainbow-mode
    scala-mode2
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Put backup files in a special directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
;; Put all auto-save files to /tmp
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Visual settings
(show-paren-mode 1)	; highlight matching parenthesis
(global-hl-line-mode 1) ; highlight current line
(line-number-mode 1)    ; show line number in modeline
(column-number-mode 1)	; show column number in modeline
(global-linum-mode 1)   ; show line numbers

;; Use Peakburn color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'peakburn t)

;; Code settings
(setq indent-tabs-mode nil) ; use spaces to indent

;; Evil (Extensible Vi Layer)
(evil-mode 1)

