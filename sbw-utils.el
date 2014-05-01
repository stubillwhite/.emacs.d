(defun sbw-utils/filter (p l)
  "Returns the items from list l for which predicate p is true."
  (delq nil
        (mapcar (lambda (x) (and (funcall p x) x)) l)))

(ert-deftest sbw-utils/filter-given-predicate-and-list-then-retains-items-for-which-predicate-is-true ()
  "sbw-utils/filter given predicate and list then retains items for which predicate is true."
  (let ((is-even? (lambda (x) (= (% x 2) 0))))
    (should (equal (sbw-utils/filter is-even? (list))         (list)))
    (should (equal (sbw-utils/filter is-even? (list 1 3 5 7)) (list)))
    (should (equal (sbw-utils/filter is-even? (list 1 2 3 4)) (list 2 4)))
    (should (equal (sbw-utils/filter is-even? (list 2 4 6 8)) (list 2 4 6 8)))))

(defun sbw-utils/assq-ensure-is-first (key alist)
  "Mutates associative alist so that the value for key is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(ert-deftest sbw-utils/assq-ensure-is-first-given-map-then-removes-duplicates-and-ensures-is-first ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw-utils/assq-ensure-is-first 'a (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw-utils/assq-ensure-is-first 'b (list a-entry b-entry c-entry)) (list b-entry a-entry c-entry)))
    (should (equal (sbw-utils/assq-ensure-is-first 'c (list a-entry b-entry c-entry)) (list c-entry a-entry b-entry)))
    (should (equal (sbw-utils/assq-ensure-is-first 'a (list a-entry b-entry a-entry)) (list a-entry b-entry)))))

(provide 'sbw-utils)
