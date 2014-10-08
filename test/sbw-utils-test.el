(require 'sbw-utils)

;; sbw/hash-table-values

(ert-deftest sbw/hash-table-values-then-returns-values ()
  (let* ( (a    (make-hash-table :test 'equal))
          (sort (-partial '-sort 'string<)))
    (puthash :k1 :a a)
    (puthash :k2 :b a)
    (should (equal (-sort 'string< (sbw/hash-table-values a)) (list :a :b)))))

;; sbw/hash-table-keys

(ert-deftest sbw/hash-table-keys-then-returns-keys ()
  (let* ( (a    (make-hash-table :test 'equal))
          (sort (-partial '-sort 'string<)))
    (puthash :k1 :a a)
    (puthash :k2 :b a)
    (should (equal (-sort 'string< (sbw/hash-table-keys a)) (list :k1 :k2)))))

;; sbw/hash-table

(ert-deftest sbw/hash-table-given-empty-initial-content-then-returns-new-empty-hash-table ()
  (should (equal (hash-table-p (sbw/hash-table)) t))
  (should (equal (hash-table-test (sbw/hash-table)) 'equal)))

(ert-deftest sbw/hash-table-given-initial-content-then-returns-new-hash-table-with-specified-content ()
  (let* ( (expected (make-hash-table :test 'equal)) )
    (puthash :k1 :a expected)
    (puthash :k2 :b expected)
    (should (sbw/hash-table-equal (sbw/hash-table :k1 :a :k2 :b) expected)))

(ert-deftest sbw/hash-table-given-invalid-initial-content-then-throws ()
  (should-error (sbw/hash-table :k1 :v1 :k2)
    :type 'wrong-number-of-arguments))

;; sbw/hash-table-equal

(ert-deftest sbw/hash-table-equal-given-hash-tables-empty-then-t ()
  (let* ( (a (make-hash-table :test 'equal))
          (b (make-hash-table :test 'equal)) )
    (should (equal (sbw/hash-table-equal a b) t)))))

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
  (let* ( (x        (sbw/hash-table :a "x" :b "y"))
          (expected (list ":b=y" ":a=x"))
          (f        (lambda (k v) (concat (symbol-name k) "=" v))) )
    (should (equal (sbw/map-hash f x) expected))))

;; sbw/decompose-time

;; TODO

;; sbw/compose-time

;; TODO

(provide 'sbw-utils-test)
