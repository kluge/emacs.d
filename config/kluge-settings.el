
;; Put backup files in a special directory
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups")))
;; Put all auto-save files to /tmp
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Add git to PATH on windows
(defvar kluge-git-path "D:/Program Files (x86)/Git/bin")
(setenv "PATH"
	(concat
	 kluge-git-path ";"
	 (getenv "PATH")))
(add-to-list 'exec-path kluge-git-path)

;; Encoding
(prefer-coding-system 'utf-8)

;; Visual settings
(show-paren-mode 1)     ; highlight matching parenthesis
(global-hl-line-mode 1) ; highlight current line
(line-number-mode 1)    ; show line number in modeline
(column-number-mode 1)  ; show column number in modeline
(global-linum-mode -1)  ; don't show line numbers

;; Behavior settings
(setq use-dialog-box nil)
(global-auto-revert-mode t) ; reflect changes on disc, if file is unchanged

;; Code settings
(setq indent-tabs-mode nil) ; use spaces to indent
; don't use tabs in align-regexp
; (from http://stackoverflow.com/questions/915985/in-emacs-how-to-line-up-equals-signs-in-a-series-of-initialization-statements)
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(provide 'kluge-settings)
