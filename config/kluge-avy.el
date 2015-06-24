;; Avy integration is heavily based on evil's ace-jump integration in evil-integration.el
(defmacro kluge-enclose-avy-for-motion (&rest body)
  "Enclose avy-jump to make it suitable for motions.
This includes restricting avy jumps to the current window in
visual and operator state, deactivating visual updates and saving
the mark."
  (declare (indent defun)
           (debug t))
  `(let ((old-mark (mark))
         (avy-all-windows
          (if (and (not (memq evil-state '(visual operator)))
                   (boundp 'avy-all-windows))
              avy-all-windows
            nil)))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         ,@body
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(evil-define-motion kluge-avy-goto-char ()
  "Move the cursor to a character selected by avy-goto-char"
  :type inclusive
  (evil-without-repeat
    (kluge-enclose-avy-for-motion
      (call-interactively 'avy-goto-char))))

(evil-define-motion kluge-avy-goto-line ()
  "Move the cursor to a line selected by avy-goto-line"
  :type line
  :repeat abort
  (evil-without-repeat
    (kluge-enclose-avy-for-motion
      (call-interactively 'avy-goto-line))))

(use-package avy
  :ensure t
  :init
  ;; Use more keys
  (setq avy-keys (append (number-sequence ?a ?z) '(?ö ?ä)))
  ;; Overlay the first character on top, but show the full path
  :config
  (define-key evil-motion-state-map (kbd "SPC") 'kluge-avy-goto-char)
  (define-key evil-motion-state-map (kbd "g SPC") 'kluge-avy-goto-line))

(provide 'kluge-avy)
