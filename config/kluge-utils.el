;; Utility functions

(defun delete-from-list (list-var item)
  "Delete item from a list variable."
  (setq list-var (delete item (symbol-value list-var))))

(provide 'kluge-utils)
