(require 'sbw-utils)
(require 'sbw-hash-tables)

;; sbw/filter

(ert-deftest sbw/filter-given-predicate-and-list-then-retains-items-for-which-predicate-is-true ()
  "sbw/filter given predicate and list then retains items for which predicate is true."
  (let ((is-even? (lambda (x) (= (% x 2) 0))))
    (should (equal (sbw/filter is-even? (list))         (list)))
    (should (equal (sbw/filter is-even? (list 1 3 5 7)) (list)))
    (should (equal (sbw/filter is-even? (list 1 2 3 4)) (list 2 4)))
    (should (equal (sbw/filter is-even? (list 2 4 6 8)) (list 2 4 6 8)))))

;; sbw/assq-ensure-is-first

(ert-deftest sbw/assq-ensure-is-first-given-map-then-removes-duplicates-and-ensures-is-first ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'b (list a-entry b-entry c-entry)) (list b-entry a-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'c (list a-entry b-entry c-entry)) (list c-entry a-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry a-entry)) (list a-entry b-entry)))))

;; sbw/assq-ensure-is-last

(ert-deftest sbw/assq-ensure-is-last-given-map-then-removes-duplicates-and-ensures-is-last ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-last 'a (list a-entry b-entry c-entry)) (list b-entry c-entry a-entry)))
    (should (equal (sbw/assq-ensure-is-last 'b (list a-entry b-entry c-entry)) (list a-entry c-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-last 'c (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-last 'a (list a-entry b-entry a-entry)) (list b-entry a-entry)))))

;; sbw/heading-one

(ert-deftest sbw/heading-one-then-returns-heading-string ()
  (should (equal (sbw/heading-one "foo") "foo\n===\n")))

;; sbw/heading-two

(ert-deftest sbw/heading-two-then-returns-heading-string ()
  (should (equal (sbw/heading-two "foo") "foo\n---\n")))

;; sbw/truncate-string

(ert-deftest sbw/truncate-string-given-string-shorter-then-returns-string-unchanged ()
  (let* ( (s "This is a test") )
    (should (equal (sbw/truncate-string s 14) "This is a test"))))

(ert-deftest sbw/truncate-string-given-string-longer-then-returns-string-truncated ()
  (let* ( (s "This is a test") )
    (should (equal (sbw/truncate-string s 13) (concat "This is a te" "\u2026")))))

;; sbw/map-hash

(ert-deftest sbw/map-hash-then-applies-function-to-values ()
  (let* ( (x        (sbw/ht-create :a "x" :b "y"))
          (expected (list ":b=y" ":a=x"))
          (f        (lambda (k v) (concat (symbol-name k) "=" v))) )
    (should (equal (sbw/map-hash f x) expected))))

(provide 'sbw-utils-test)
