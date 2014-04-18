(use-package google-this
  :commands (google-this google-this-mode google-error)
  :init
  (evil-leader/set-key "gg" 'google-this))

(provide 'kluge-google-this)
