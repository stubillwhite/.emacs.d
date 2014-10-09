(require 'sbw-hash-table-utils)

;; sbw/ht-keys

(ert-deftest sbw/ht-keys-then-keys ()
  (let* ( (a (sbw/ht-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht-keys a)) (list :k1 :k2)))))

;; sbw/ht-vals

(ert-deftest sbw/ht-vals-then-values ()
  (let* ( (a (sbw/ht-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht-vals a)) (list :a :b)))))

;; sbw/ht-equal

(ert-deftest sbw/ht-equal-given-hash-tables-empty-then-t ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (should (equal (sbw/ht-equal a b) t))))

(ert-deftest sbw/ht-equal-given-hash-tables-equal-then-t ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k 1 a)
    (puthash :k 1 b)
    (should (equal (sbw/ht-equal a b) t))))

(ert-deftest sbw/ht-equal-given-keys-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k2 1 b)
    (should (equal (sbw/ht-equal a b) nil))))

(ert-deftest sbw/ht-equal-given-values-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k1 2 b)
    (should (equal (sbw/ht-equal a b) nil))))

(ert-deftest sbw/ht-equal-given-keys-and-values-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal))
          (c (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k2 2 b)
    (puthash :k1 1 b)
    (puthash :k2 2 c)
    (should (equal (sbw/ht-equal a b) nil))
    (should (equal (sbw/ht-equal b c) nil))
    (should (equal (sbw/ht-equal c a) nil))))

;; sbw/ht-create

(ert-deftest sbw/ht-create-given-empty-initial-content-then-returns-new-empty-hash-table ()
  (should (equal (hash-table-p    (sbw/ht-create)) t))
  (should (equal (hash-table-test (sbw/ht-create)) 'equal)))

(ert-deftest sbw/ht-create-given-initial-content-then-returns-new-hash-table-with-specified-content ()
  (let* ( (expected (make-hash-table :test 'equal)) )
    (puthash :k1 :a expected)
    (puthash :k2 :b expected)
    (should (sbw/ht-equal (sbw/ht-create :k1 :a :k2 :b) expected))))

(ert-deftest sbw/ht-create-given-invalid-initial-content-then-throws ()
  (should-error (sbw/ht-create :k1 :v1 :k2)
    :type 'wrong-number-of-arguments))

;; sbw/ht-get

(ert-deftest sbw/ht-get-given-value-exists-then-associated-value ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-get hash-table :k) :a))))

(ert-deftest sbw/ht-get-given-value-does-not-exist-then-nil ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-get hash-table :x) nil))))

;; sbw/ht-contains?

(ert-deftest sbw/ht-contains?-given-value-exists-then-t ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-contains? hash-table :k) t))))

(ert-deftest sbw/ht-get-given-value-does-not-exist-then-nil ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-contains? hash-table :x) nil))))

;; sbw/ht-copy

(ert-deftest sbw/ht-copy-then-copy-of-hash-table ()
  (let* ( (hash-table (sbw/ht-create :k :a))
          (copy (sbw/ht-copy hash-table)) )
    (should (sbw/ht-equal copy hash-table))
    (remhash :k hash-table)
    (should-not (sbw/ht-equal copy hash-table))))

;; sbw/merge

(ert-deftest sbw/ht-merge-then-merged-copy-of-hash-tables ()
  (let* ( (a        (sbw/ht-create :x :a1 :y :a2))
          (b        (sbw/ht-create        :y :b2 :z :b3))
          (c        (sbw/ht-create               :z :c3))
          (expected (sbw/ht-create :x :a1 :y :b2 :z :c3)) )
    (should (sbw/ht-equal (sbw/ht-merge a b c) expected))))

;; sbw/assoc

(ert-deftest sbw/ht-assoc-given-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k :v1))
          (expected   (sbw/ht-create :k :v2)) )
    (should (sbw/ht-equal (sbw/ht-assoc hash-table :k :v2) expected))))

(ert-deftest sbw/ht-assoc-given-new-key-then-adds ()
  (let* ( (hash-table (sbw/ht-create :x :a))
          (expected   (sbw/ht-create :x :a :y :b)) )
    (should (sbw/ht-equal (sbw/ht-assoc hash-table :y :b) expected))))

(provide 'sbw-hash-table-utils-test)
