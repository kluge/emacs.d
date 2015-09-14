(defconst emacs-start-time (current-time))

;; Disable extraneous GUI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;; Set frame size
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 120))

;; Set font size
(if (or (eq system-type 'windows-nt) (file-exists-p "~/.emacs.d/big-text"))
    (set-face-attribute 'default nil :height 100)
  (set-face-attribute 'default nil :height 85))

;; Show current buffer in frame title
(setq frame-title-format '("" "%b - Emacs"))

;; Use Peakburn color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'peakburn t)

;; Set encoding (has to happen before package loading on Windows)
(prefer-coding-system 'utf-8)

;; Set up the package system
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil) ; don't run package-initialize again after loading init.el
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load config from separate files
(add-to-list 'load-path "~/.emacs.d/config/")

;; Unpackaged elisp
(add-to-list 'load-path "~/.emacs.d/vendor/")

;; Save customizations to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; use-package
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.05)

;; General settings
(require 'kluge-settings)

;; Utility functions
(require 'kluge-utils)

;; Evil (Extensible Vi Layer)
(require 'kluge-evil)

;; Ag (Silver searcher)
(require 'kluge-ag)

;; Ansi-term
(require 'kluge-ansi-term)

;; Auctex
(require 'kluge-auctex)

;; Avy (replacement for ace-jump)
(require 'kluge-avy)

;; YASnippet
(require 'kluge-yasnippet)

;; Company
(require 'kluge-company)

;; C++
(require 'kluge-cpp)

;; Dired
(require 'kluge-dired)

;; Emacs Lisp
(require 'kluge-elisp)

;; Eshell
(require 'kluge-eshell)

;; Expand region
(require 'kluge-expand-region)

;; Flycheck
(require 'kluge-flycheck)

;; Gtags (ggtags and helm-gtags)
(require 'kluge-gtags)

;; Google this
(require 'kluge-google-this)

(require 'kluge-ivy)

;; Haskell
(require 'kluge-haskell)

;; Helm
(require 'kluge-helm)

;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :diminish highlight-symbol-mode
  :config
  ;; Highlight symbol in programming modes
  (add-hook 'prog-mode-hook 'highlight-symbol-mode))

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

;; Paradox (a better package menu)
(require 'kluge-paradox)

;; Scala
(require 'kluge-scala)

;; Smart-mode-line
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

;; Smartparens
(require 'kluge-smartparens)

;; Uniquify
(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'reverse))

;; Which-key
(use-package which-key
  :ensure t
  :defer 5
  :diminish which-key-mode
  :config
  (which-key-mode))

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

;; Menu key for M-x
(global-set-key (kbd "<apps>") 'execute-extended-command)

;; Diminish
(use-package diminish
  :ensure t
  :config
  (progn
    ;; Clean up modes that don't need to show on the modeline
    (diminish 'projectile-mode)
    (diminish 'undo-tree-mode)))

;; Change to %HOME% on windows
(when (eq system-type 'windows-nt)
    (cd (getenv "HOME")))

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

