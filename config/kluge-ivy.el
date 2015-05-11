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
  ;; Switch RET and C-j
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-done)

  ;; Ex bindings for finding files and switching buffers
  (define-key evil-ex-map "b " 'switch-to-buffer)
  (define-key evil-ex-map "e " 'find-file)

  ;; And leader bindings
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "f" 'find-file)
  (ivy-mode 1)

  ;; Swiper
  (global-set-key (kbd "C-s") 'kluge-swiper))

(provide 'kluge-ivy)
