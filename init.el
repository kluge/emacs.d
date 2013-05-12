
; Disable menu and tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)

; Set up the package system
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

; Install the packages, if they're not already installed
(defvar my-packages '(evil))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

; Evil (Extensible Vi Layer)
(evil-mode 1)

; Settings
(show-paren-mode 1) ; highlight matching parenthesis
