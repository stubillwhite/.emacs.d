(defun sbw/assq-ensure-is-first (key alist)
  "Mutates associative ALIST so that the value for KEY is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(defun sbw/assq-ensure-is-last (key alist)
  "Mutates associative ALIST so that the value for KEY is last."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry :append)))

(defun sbw/heading-one (s)
  "Returns string S formatted to be a top level heading."
  (concat s "\n" (make-string (length s) ?=) "\n"))

(defun sbw/heading-two (s)
  "Returns string S formatted to be a sub-heading."
  (concat s "\n" (make-string (length s) ?-) "\n"))

(defun sbw/truncate-string (s n)
  "Returns S truncated to N characters with ellipsis if truncation occurred."
  (if (> (length s) n)
    (concat (substring s 0 (- n 1)) "\u2026")
    s))

;; TODO Test this
;; TODO Might be replaced with s.el now?
(defun sbw/collect-by (f l)
  "Returns a hash-table of lists of items, keyed by the result of (f item) for each item in list l."
  (-reduce-from    
    (lambda (acc val)
      (let* ( (category (funcall f val))
              (curr-val (gethash category acc (list))) )
        (puthash category (cons val curr-val) acc)
        acc))
    (sbw/ht-create)
    l))

(defalias 'sbw/dec '1- "Return x minus one.")
(defalias 'sbw/inc '1+ "Return x plus one.")

(provide 'sbw-utils)
