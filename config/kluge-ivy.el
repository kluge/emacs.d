(defun kluge-swiper ()
  "Add swiper searches to the search ring to allow moving between
  matches with n and N afterwards."
  (interactive)
  (swiper)
  (setq isearch-forward t) ; treat swiper searches always as forward searches
  (add-to-list 'regexp-search-ring (ivy--regex ivy-text)))

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-height 20)
  (setq ivy-wrap t) ; wrap after first and last candidate
  ;; Switch RET and C-j
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  ;; Ex bindings for finding files and switching buffers
  (define-key evil-ex-map "b " 'switch-to-buffer)

  ;; And leader bindings
  (evil-leader/set-key
    "b" 'switch-to-buffer)
  (ivy-mode 1)

  ;; Swiper
  (global-set-key (kbd "C-s") 'kluge-swiper))

(use-package counsel
  :ensure t
  :commands (counsel-M-x counsel-find-file)
  :init
  ;; Ignore files starting with # or .
  (setq counsel-find-file-ignore-regexp "\\(\\`[#.]\\)")

  ;; Menu key for M-x on Linux and Windows
  (global-set-key (kbd "<menu>") 'counsel-M-x)
  (global-set-key (kbd "<apps>") 'counsel-M-x)
  (global-set-key (kbd "M-x") 'counsel-M-x)

  ;; When smex is installed, counsel-M-x uses it for sorting
  (use-package smex :ensure t)

  (define-key evil-ex-map "e " 'counsel-find-file)
  (evil-leader/set-key
    "f" 'counsel-find-file))

(provide 'kluge-ivy)
