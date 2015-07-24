;; Utility functions

(defun delete-from-list (list-var item)
  "Delete item from a list variable."
  (set list-var (delete item (symbol-value list-var))))

(provide 'kluge-utils)
