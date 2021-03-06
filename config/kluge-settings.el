
;; Put backup files in a special directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
;; Put all auto-save files to /tmp
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
;; Avoid loading outdated bytecompiled files
(unless (version< emacs-version "24.4")
  (setq load-prefer-newer t))

;; Add git to PATH on windows
(when (eq system-type 'windows-nt)
  (defvar kluge-git-path "D:/Program Files (x86)/Git/bin")
  (setenv "PATH"
	  (concat
	   kluge-git-path ";"
	   (getenv "PATH")))
  (add-to-list 'exec-path kluge-git-path))

(when (eq system-type 'gnu/linux)
  (let ((local-bin "~/.local/bin"))
    (setenv "PATH"
            (concat local-bin ";" (getenv "PATH")))
    (add-to-list 'exec-path local-bin)))

;; Encoding
(prefer-coding-system 'utf-8)

;; Visual settings
(show-paren-mode 1)     ; highlight matching parenthesis
(global-hl-line-mode 1) ; highlight current line
(line-number-mode 1)    ; show line number in modeline
(column-number-mode 1)  ; show column number in modeline
(global-linum-mode -1)  ; don't show line numbers

(setq max-mini-window-height 0.5) ; minibuffer can be bigger if necessary

(setq initial-scratch-message "") ; I know what *scratch* is for

;; Behavior settings
(setq use-dialog-box nil)
(global-auto-revert-mode t) ; reflect changes on disc, if file is unchanged
; Also autorefresh dired without too much ceremony
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ask for the compilation command
(setq compilation-read-command nil)

;; Code settings
(set-default 'sentence-end-double-space nil) ; sentences don't need two spaces to end
(setq-default indent-tabs-mode nil) ; use spaces to indent
; don't use tabs in align-regexp
; (from http://stackoverflow.com/questions/915985/in-emacs-how-to-line-up-equals-signs-in-a-series-of-initialization-statements)
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

;; Remember place in files
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saved-places")
(setq-default save-place t)

(provide 'kluge-settings)








