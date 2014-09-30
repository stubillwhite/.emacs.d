(defun sbw/filter (p l)
  "Returns the items from list l for which predicate p is true."
  (delq nil
        (mapcar (lambda (x) (and (funcall p x) x)) l)))

(ert-deftest sbw/filter-given-predicate-and-list-then-retains-items-for-which-predicate-is-true ()
  "sbw/filter given predicate and list then retains items for which predicate is true."
  (let ((is-even? (lambda (x) (= (% x 2) 0))))
    (should (equal (sbw/filter is-even? (list))         (list)))
    (should (equal (sbw/filter is-even? (list 1 3 5 7)) (list)))
    (should (equal (sbw/filter is-even? (list 1 2 3 4)) (list 2 4)))
    (should (equal (sbw/filter is-even? (list 2 4 6 8)) (list 2 4 6 8)))))

(defun sbw/assq-ensure-is-first (key alist)
  "Mutates associative alist so that the value for key is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(ert-deftest sbw/assq-ensure-is-first-given-map-then-removes-duplicates-and-ensures-is-first ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'b (list a-entry b-entry c-entry)) (list b-entry a-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'c (list a-entry b-entry c-entry)) (list c-entry a-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry a-entry)) (list a-entry b-entry)))))

(defun sbw/hash-table ()
  "Returns a new hash-table with equal comparator."
  (make-hash-table :test 'equal))

(ert-deftest sbw/hash-table-then-returns-new-hash-table-with-equals-comparator ()
  (should (equal (hash-table-p (sbw/hash-table)) t))
  (should (equal (hash-table-test (sbw/hash-table)) 'equal)))

(defun sbw/heading-one (s)
  (concat s "\n" (make-string (length s) ?=) "\n"))

(ert-deftest sbw/heading-one-then-returns-heading-string ()
  (should (equal (sbw/heading-one "foo") "foo\n===\n")))

(defun sbw/heading-two (s)
  (concat s "\n" (make-string (length s) ?-) "\n"))

(ert-deftest sbw/heading-two-then-returns-heading-string ()
  (should (equal (sbw/heading-two "foo") "foo\n---\n")))

(defun sbw/map-hash (f hash-table)
  "Returns a list of the result of calling f on each key value entry in the specified hash-table."
  (let ( (results (list)) )
    (maphash (lambda (k v) (push (funcall f k v) results)) hash-table)
    results))

;; TODO Test this
(defun sbw/map-hash (f hash-table)
  "Returns a list of the result of calling f on each key value entry in the specified hash-table."
  (let ( (results) )
    (maphash (lambda (k v) (push (funcall f k v) results)) hash-table)
    results
    ))

;; TODO Test this
(defun sbw/collect-by (f l)
  "Returns a hash-table of lists of items, keyed by the result of (f item) for each item in list l."
  (-reduce-from    
    (lambda (acc val)
      (let* ( (category (funcall f val))
              (curr-val (gethash category acc (list))) )
        (puthash category (cons val curr-val) acc)
        acc))
    (sbw/hash-table)
    l))

;; TODO Test these, break tests into a separate file
(defun sbw/join (sep list)
  "Returns a string with the items from list joined by separator."
  (apply 'concat (-drop 1 (-interleave (-repeat (length list) sep) list))))

(defalias '-dec '1- "Return x minus one.")
(defalias '-inc '1+ "Return x plus one.")

(defun sbw/hash-table-values (hash-table)
  "Returns the values from HASH-TABLE."
  (let* ( (values ()) )
    (maphash (lambda (k v) (push v values)) hash-table)
    values))

(defun sbw/hash-table-keys (hash-table)
  "Returns the keys from HASH-TABLE."
  (let* ( (keys ()) )
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(provide 'sbw-utils)
