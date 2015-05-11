(use-package avy
  :ensure t
  :init
  ;; Use more keys
  (setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?ö ?q ?w ?e ?r ?u ?i
		      ?o ?p ?g ?t ?h ?y ?z ?x ?c ?v ?m ?, ?. ?b ?n)) 
  ;; Overlay the first character on top, but show the full path
  (setq avy-style 'at-full)
  :config
  (define-key evil-motion-state-map (kbd "SPC") 'avy-goto-char)
  (define-key evil-motion-state-map (kbd "g SPC") 'avy-goto-line))

(provide 'kluge-avy)
