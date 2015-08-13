(require 'sbw-hash-tables-two)
(require 'sbw-value-eq)

;; sbw/ht2-create

(ert-deftest sbw/ht2-create-given-empty-initial-content-then-new-empty-hash-table ()
  (should (equal (hash-table-p    (sbw/ht2-create)) t))
  (should (equal (hash-table-test (sbw/ht2-create)) 'sbw/value-eq-test)))
 
(ert-deftest sbw/ht2-create-given-initial-content-then-new-hash-table-with-specified-content ()
  (let* ( (expected (make-hash-table :test 'equal)) )
    (puthash :k1 :a expected)
    (puthash :k2 :b expected)
    (should (sbw/value-eq (sbw/ht2-create :k1 :a :k2 :b) expected))))
 
(ert-deftest sbw/ht2-create-given-invalid-initial-content-then-throws ()
  (should-error (sbw/ht2-create :k1 :v1 :k2)
    :type 'wrong-number-of-arguments))

(ert-deftest sbw/ht2-create-with-comparator-then-returns-hash-table-using-comparator ()
  (should (equal (hash-table-test (sbw/ht2-create-with-comparator 'eq)) 'eq)))

;; sbw/ht2-keys

(ert-deftest sbw/ht2-keys-then-keys ()
  (let* ( (a (sbw/ht2-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht2-keys a)) (list :k1 :k2)))))

;; sbw/ht2-vals

(ert-deftest sbw/ht2-vals-then-values ()
  (let* ( (a (sbw/ht2-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht2-vals a)) (list :a :b)))))

;; sbw/ht2-get

(ert-deftest sbw/ht2-get-given-value-exists-then-associated-value ()
  (let* ( (hash-table (sbw/ht2-create :k :a)) )
    (should (equal (sbw/ht2-get hash-table :k) :a))))

(ert-deftest sbw/ht2-get-given-value-does-not-exist-and-no-default-then-nil ()
  (let* ( (hash-table (sbw/ht2-create :k :a)) )
    (should (equal (sbw/ht2-get hash-table :x) nil))))

(ert-deftest sbw/ht2-get-given-value-does-not-exist-and-default-then-default ()
  (let* ( (hash-table (sbw/ht2-create :k :a)) )
    (should (equal (sbw/ht2-get hash-table :x :default) :default))))

;; sbw/ht2-get-in

(ert-deftest sbw/ht2-get-in-given-root-key-does-not-exist-then-nil ()
  (let* ( (hash-table (sbw/ht2-create)) )
    (should (equal (sbw/ht2-get-in hash-table [:k1]) nil))))

(ert-deftest sbw/ht2-get-in-given-nested-key-does-not-exist-key-then-nil ()
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v))) )
    (should (equal (sbw/ht2-get-in hash-table [:k1 :k3]) nil))))

(ert-deftest sbw/ht2-get-in-given-path-through-non-nested-structure-then-error ()
  (let* ( (hash-table (sbw/ht2-create :k1 :k2)) )
    (should-error (sbw/ht2-get-in hash-table [:k1 :k2]) :type 'wrong-type-argument)))

(ert-deftest sbw/ht2-get-in-given-root-key-exists-then-value ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v)) )
    (should (equal (sbw/ht2-get-in hash-table [:k1]) :v))))

(ert-deftest sbw/ht2-get-in-given-nested-key-exists-then-value ()
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v))) )
    (should (equal (sbw/ht2-get-in hash-table [:k1 :k2]) :v))))

(ert-deftest sbw/ht2-get-in-given-root-key-does-not-exist-and-default-then-default ()
  (let* ( (hash-table (sbw/ht2-create)) )
    (should (equal (sbw/ht2-get-in hash-table [:k1] :d) :d))))

(ert-deftest sbw/ht2-get-in-given-nested-key-does-not-exist-key-and-default-then-default ()
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v))) )
    (should (equal (sbw/ht2-get-in hash-table [:k1 :k3] :d) :d))))

;; sbw/ht2-contains?

(ert-deftest sbw/ht2-contains?-given-value-exists-then-t ()
  (let* ( (hash-table (sbw/ht2-create :k :a)) )
    (should (equal (sbw/ht2-contains? hash-table :k) t))))

(ert-deftest sbw/ht2-get-given-value-does-not-exist-then-nil ()
  (let* ( (hash-table (sbw/ht2-create :k :a)) )
    (should (equal (sbw/ht2-contains? hash-table :x) nil))))

;; sbw/ht2-copy

(ert-deftest sbw/ht2-copy-then-copy-of-hash-table ()
  (let* ( (hash-table (sbw/ht2-create :k :a))
          (copy (sbw/ht2-copy hash-table)) )
    (should (sbw/value-eq copy hash-table))
    (remhash :k hash-table)
    (should-not (sbw/value-eq copy hash-table))))

;; sbw/ht2-merge

(ert-deftest sbw/ht2-merge-then-merged-copy-of-hash-tables ()
  (let* ( (a        (sbw/ht2-create :x :a1 :y :a2       ))
          (b        (sbw/ht2-create        :y :b2 :z :b3))
          (c        (sbw/ht2-create               :z :c3))
          (expected (sbw/ht2-create :x :a1 :y :b2 :z :c3)) )
    (should (sbw/value-eq (sbw/ht2-merge a b c) expected))))

;; sbw/ht2-assoc

(ert-deftest sbw/ht2-assoc-given-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht2-create :k :v1))
          (expected   (sbw/ht2-create :k :v2)) )
    (should (sbw/value-eq (sbw/ht2-assoc hash-table :k :v2) expected))))

(ert-deftest sbw/ht2-assoc-given-new-key-then-adds ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (expected   (sbw/ht2-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-assoc hash-table :k2 :v2) expected))))

;; sbw/ht2-dissoc

(ert-deftest sbw/ht2-dissoc-given-key-exists-then-removes ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1 :k2 :v2))
          (expected   (sbw/ht2-create         :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-dissoc hash-table :k1) expected))))

(ert-deftest sbw/ht2-dissoc-given-key-does-not-exist-then-unchanged ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1 :k2 :v2))
          (expected   (sbw/ht2-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-dissoc hash-table :x) expected))))

;; sbw/ht2-select-keys

(ert-deftest sbw/ht2-select-keys-then-retains-selected ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1 :k2 :v2 :k3 :v3))
          (expected   (sbw/ht2-create         :k2 :v2 :k3 :v3)) )
    (should (sbw/value-eq (sbw/ht2-select-keys hash-table [:k2 :k3 :k4]) expected))))

;; sbw/ht2-zipmap

(ert-deftest sbw/ht2-zipmap-given-empty-keys-and-vaulues-then-new-empty-hash-table ()
  (should (sbw/value-eq (sbw/ht2-zipmap '() '()) (sbw/ht2-create))))

(ert-deftest sbw/ht2-zipmap-given-keys-and-values-then-then-new-hash-table-with-content ()
  (let* ( (expected   (sbw/ht2-create :a 1 :b 2 :c 3)) )
    (should (sbw/value-eq (sbw/ht2-zipmap '(:a :b :c) '(1 2 3)) expected))))

;; sbw/ht2-map-vals

(ert-deftest sbw/ht2-map-vals-then-updates-values ()
  (let* ( (hash-table (sbw/ht2-create :a 1 :b 2 :c 3))
          (inc-val    '(lambda (x) (+ x 1)))
          (expected   (sbw/ht2-create :a 2 :b 3 :c 4)) )
    (should (sbw/value-eq (sbw/ht2-map-vals inc-val hash-table) expected))))

;; sbw/ht2-update

(ert-deftest sbw/ht2-update-given-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht2-create :k 1))
          (inc        (lambda (x) (+ 1 x)))
          (expected   (sbw/ht2-create :k 2)) )
    (should (sbw/value-eq (sbw/ht2-update hash-table :k inc) expected))))

(ert-deftest sbw/ht2-update-given-new-key-then-adds ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht2-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-update hash-table :k2 f) expected))))

;; sbw/ht2-assoc-in

(ert-deftest sbw/ht2-assoc-in-given-root-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (expected   (sbw/ht2-create :k1 :v2)) )
    (should (sbw/value-eq (sbw/ht2-assoc-in hash-table [:k1] :v2) expected))))

(ert-deftest sbw/ht2-assoc-in-given-root-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (expected   (sbw/ht2-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-assoc-in hash-table [:k2] :v2) expected))))

(ert-deftest sbw/ht2-assoc-in-given-nested-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1)))
          (expected   (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v2))) )
    (should (sbw/value-eq (sbw/ht2-assoc-in hash-table [:k1 :k2] :v2) expected))))

(ert-deftest sbw/ht2-assoc-in-given-nested-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1)))
          (expected   (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1 :k3 :v2))) )
    (should (sbw/value-eq (sbw/ht2-assoc-in hash-table [:k1 :k3] :v2) expected))))

(ert-deftest sbw/ht2-assoc-in-given-path-through-existing-non-nested-structure-then-throws ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1)) )
    (should-error (sbw/ht2-assoc-in hash-table [:k1 :k2] :v1) :type 'wrong-type-argument)))

(ert-deftest sbw/ht2-assoc-in-given-path-through-non-existent-key-then-adds-empty-hash-table ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (expected   (sbw/ht2-create :k1 :v1 :k2 (sbw/ht-create :k3 (sbw/ht-create :k4 :v2)))) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k2 :k3 :k4] :v2) expected))))

;; sbw/ht2-update-in

(ert-deftest sbw/ht2-update-in-given-root-key-exists-then-replaces ()
  :expected-result :failed
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (f          (lambda (x) (when (equal x :v1) :v2)))
          (expected   (sbw/ht2-create :k1 :v2)) )
    (should (sbw/value-eq (sbw/ht2-update-in hash-table [:k1] f) expected))))

(ert-deftest sbw/ht2-update-in-given-root-key-does-not-exist-then-adds ()
  :expected-result :failed
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht2-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht2-update-in hash-table [:k2] f) expected))))

(ert-deftest sbw/ht2-update-in-given-nested-key-exists-then-replaces ()
  :expected-result :failed
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1)))
          (f          (lambda (x) (when (equal x :v2) :v3)))
          (expected   (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v3))) )
    (should (sbw/value-eq (sbw/ht2-update-in hash-table [:k1 :k2] f) expected))))

(ert-deftest sbw/ht2-update-in-given-nested-key-does-not-exist-then-adds ()
  :expected-result :failed
  (let* ( (hash-table (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1)))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht2-create :k1 (sbw/ht2-create :k2 :v1 :k3 :v2))) )
    (should (sbw/value-eq (sbw/ht2-update-in hash-table [:k1 :k3] f) expected))))

(ert-deftest sbw/ht2-update-in-given-path-through-existing-non-nested-structure-then-throws ()
  (let* ( (hash-table (sbw/ht2-create :k1 :v1)) )
    (should-error (sbw/ht2-update-in hash-table [:k1 :k2] :v1) :type 'wrong-type-argument)))

(ert-deftest sbw/ht2-update-in-given-path-through-non-existent-key-then-adds-empty-hash-table ()
  :expected-result :failed
  (let* ( (hash-table (sbw/ht2-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht2-create :k1 :v1 :k2 (sbw/ht-create :k3 (sbw/ht-create :k4 :v2)))) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k2 :k3 :k4] f) expected))))

(provide 'sbw-hash-tables-two-test)
