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

(defun sbw/interleave (l1 l2)
  "Returns a list of the first item in l1 and l2, then the second, etc. The shortest list will stop the interleaving."  
  (if (eql l1 nil)
    nil
    (cons (first l1) (sbw/interleave l2 (rest l1)))))

(ert-deftest sbw/interleave-then-interleves-two-lists ()
  (should (equal (sbw/interleave (list)     (list 1 2))   (list)))
  (should (equal (sbw/interleave (list 1 2) (list 3 4 5)) (list 1 3 2 4)))
  (should (equal (sbw/interleave (list 1 2) (list))       (list 1))))

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

(defun sbw/map-hash (f hash-table)
  "Returns a list of the result of calling f on each key value entry in the specified hash-table."
  (let ( (results) )
    (maphash (lambda (k v) (push (funcall f k v) results)) hash-table)
    results
    ))

;; TODO Test this

(provide 'sbw-utils)
