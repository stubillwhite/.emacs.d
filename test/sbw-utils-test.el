(require 'sbw-utils)

;; sbw/hash-table-values



;; sbw/hash-table-equal

(ert-deftest sbw/hash-table-equal-given-hash-tables-empty-then-t ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (should (equal (sbw/hash-table-equal a b) t))))

(ert-deftest sbw/hash-table-equal-given-hash-tables-equal-then-t ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k 1 a)
    (puthash :k 1 b)
    (should (equal (sbw/hash-table-equal a b) t))))

(ert-deftest sbw/hash-table-equal-given-keys-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k2 1 b)
    (should (equal (sbw/hash-table-equal a b) nil))))

(ert-deftest sbw/hash-table-equal-given-values-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k1 2 b)
    (should (equal (sbw/hash-table-equal a b) nil))))

(ert-deftest sbw/hash-table-equal-given-keys-and-values-differ-then-nil ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal))
          (c (make-hash-table :test 'equal)) )
    (puthash :k1 1 a)
    (puthash :k1 1 b)
    (puthash :k2 2 b)
    (puthash :k2 2 c)
    (should (equal (sbw/hash-table-equal a b) nil))
    (should (equal (sbw/hash-table-equal b c) nil))
    (should (equal (sbw/hash-table-equal c a) nil))))

(provide 'sbw-utils-test)
