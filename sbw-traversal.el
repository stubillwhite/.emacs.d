(require 'dash)


(defun sbw/traverse (f acc obj)
  (cond
    ((stringp obj)      (f :sbw/traverse-string acc obj))
    ((keywordp obj)     (f :sbw/traverse-keyword acc obj))
    ((symbolp obj)      (f :sbw/traverse-symbol acc obj))
    ((numberp obj)      (f :sbw/traverse-number acc obj))
    ((arrayp obj)       (-reduce-from (lamda (acc v) (sbw/traverse f acc v)) acc obj))
    ((hash-table-p obj) )
    ((listp obj)        (-reduce-from (lamda (acc v) (sbw/traverse f acc v)) acc obj))
    (t                  (signal 'json-error (list obj))))
  )







(provide 'sbw/traversal)
