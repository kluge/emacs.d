(require 'term)

;; Don't enable yasnippet in ansi-term
(add-hook 'term-mode-hook (lambda ()
			    (yas-minor-mode -1)))

;; Use emacs state in ansi-term
(evil-set-initial-state 'term-mode 'emacs)

;; Adapted from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(defun kluge-visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, switch to previous buffer
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunct one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/zsh")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (switch-to-prev-buffer)
              (if anon-term
                  (switch-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (switch-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))
(global-set-key (kbd "<f2>") 'kluge-visit-ansi-term)

(defun kluge-split-ansi-term ()
  (interactive)
  (kluge-horizontal-split)
  (kluge-visit-ansi-term))
(define-key evil-window-map (kbd "<f2>") 'kluge-split-ansi-term)



(provide 'kluge-ansi-term)
