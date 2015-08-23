(require 'sbw-hash-tables)
(require 'sbw-value-eq)

;; sbw/ht-create

(ert-deftest sbw/ht-create-given-empty-initial-content-then-new-empty-hash-table ()
  (should (equal (hash-table-p    (sbw/ht-create)) t))
  (should (equal (hash-table-test (sbw/ht-create)) 'sbw/value-eq-test)))
 
(ert-deftest sbw/ht-create-given-initial-content-then-new-hash-table-with-specified-content ()
  (let* ( (expected (make-hash-table :test 'equal)) )
    (puthash :k1 :a expected)
    (puthash :k2 :b expected)
    (should (sbw/value-eq (sbw/ht-create :k1 :a :k2 :b) expected))))
 
(ert-deftest sbw/ht-create-given-invalid-initial-content-then-throws ()
  (should-error (sbw/ht-create :k1 :v1 :k2)
    :type 'wrong-number-of-arguments))

(ert-deftest sbw/ht-create-with-comparator-then-returns-hash-table-using-comparator ()
  (should (equal (hash-table-test (sbw/ht-create-with-comparator 'eq)) 'eq)))

;; sbw/ht-keys

(ert-deftest sbw/ht-keys-then-keys ()
  (let* ( (a (sbw/ht-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht-keys a)) (list :k1 :k2)))))

;; sbw/ht-vals

(ert-deftest sbw/ht-vals-then-values ()
  (let* ( (a (sbw/ht-create :k1 :a :k2 :b)) )
    (should (equal (-sort 'string< (sbw/ht-vals a)) (list :a :b)))))

;; sbw/ht-get

(ert-deftest sbw/ht-get-given-value-exists-then-associated-value ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-get hash-table :k) :a))))

(ert-deftest sbw/ht-get-given-value-does-not-exist-and-no-default-then-nil ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-get hash-table :x) nil))))

(ert-deftest sbw/ht-get-given-value-does-not-exist-and-default-then-default ()
  (let* ( (hash-table (sbw/ht-create :k :a)) )
    (should (equal (sbw/ht-get hash-table :x :default) :default))))

;; sbw/ht-get-in

(ert-deftest sbw/ht-get-in-given-root-key-does-not-exist-then-nil ()
  (let* ( (hash-table (sbw/ht-create)) )
    (should (equal (sbw/ht-get-in hash-table [:k1]) nil))))

(ert-deftest sbw/ht-get-in-given-nested-key-does-not-exist-key-then-nil ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v))) )
    (should (equal (sbw/ht-get-in hash-table [:k1 :k3]) nil))))

(ert-deftest sbw/ht-get-in-given-path-through-non-nested-structure-then-error ()
  (let* ( (hash-table (sbw/ht-create :k1 :k2)) )
    (should-error (sbw/ht-get-in hash-table [:k1 :k2]) :type 'wrong-type-argument)))

(ert-deftest sbw/ht-get-in-given-root-key-exists-then-value ()
  (let* ( (hash-table (sbw/ht-create :k1 :v)) )
    (should (equal (sbw/ht-get-in hash-table [:k1]) :v))))

(ert-deftest sbw/ht-get-in-given-nested-key-exists-then-value ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v))) )
    (should (equal (sbw/ht-get-in hash-table [:k1 :k2]) :v))))

(ert-deftest sbw/ht-get-in-given-root-key-does-not-exist-and-default-then-default ()
  (let* ( (hash-table (sbw/ht-create)) )
    (should (equal (sbw/ht-get-in hash-table [:k1] :d) :d))))

(ert-deftest sbw/ht-get-in-given-nested-key-does-not-exist-key-and-default-then-default ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v))) )
    (should (equal (sbw/ht-get-in hash-table [:k1 :k3] :d) :d))))

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
    (should (sbw/value-eq copy hash-table))
    (remhash :k hash-table)
    (should-not (sbw/value-eq copy hash-table))))

;; sbw/ht-merge

(ert-deftest sbw/ht-merge-then-merged-copy-of-hash-tables ()
  (let* ( (a        (sbw/ht-create :x :a1 :y :a2       ))
          (b        (sbw/ht-create        :y :b2 :z :b3))
          (c        (sbw/ht-create               :z :c3))
          (expected (sbw/ht-create :x :a1 :y :b2 :z :c3)) )
    (should (sbw/value-eq (sbw/ht-merge a b c) expected))))

;; sbw/ht-assoc

(ert-deftest sbw/ht-assoc-given-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k :v1))
          (expected   (sbw/ht-create :k :v2)) )
    (should (sbw/value-eq (sbw/ht-assoc hash-table :k :v2) expected))))

(ert-deftest sbw/ht-assoc-given-new-key-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (expected   (sbw/ht-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-assoc hash-table :k2 :v2) expected))))

;; sbw/ht-dissoc

(ert-deftest sbw/ht-dissoc-given-key-exists-then-removes ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1 :k2 :v2))
          (expected   (sbw/ht-create         :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-dissoc hash-table :k1) expected))))

(ert-deftest sbw/ht-dissoc-given-key-does-not-exist-then-unchanged ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1 :k2 :v2))
          (expected   (sbw/ht-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-dissoc hash-table :x) expected))))

;; sbw/ht-select-keys

(ert-deftest sbw/ht-select-keys-then-retains-selected ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1 :k2 :v2 :k3 :v3))
          (expected   (sbw/ht-create         :k2 :v2 :k3 :v3)) )
    (should (sbw/value-eq (sbw/ht-select-keys hash-table [:k2 :k3 :k4]) expected))))

;; sbw/ht-zipmap

(ert-deftest sbw/ht-zipmap-given-empty-keys-and-vaulues-then-new-empty-hash-table ()
  (should (sbw/value-eq (sbw/ht-zipmap '() '()) (sbw/ht-create))))

(ert-deftest sbw/ht-zipmap-given-keys-and-values-then-then-new-hash-table-with-content ()
  (let* ( (expected   (sbw/ht-create :a 1 :b 2 :c 3)) )
    (should (sbw/value-eq (sbw/ht-zipmap '(:a :b :c) '(1 2 3)) expected))))

;; sbw/ht-map-vals

(ert-deftest sbw/ht-map-vals-then-updates-values ()
  (let* ( (hash-table (sbw/ht-create :a 1 :b 2 :c 3))
          (inc-val    '(lambda (x) (+ x 1)))
          (expected   (sbw/ht-create :a 2 :b 3 :c 4)) )
    (should (sbw/value-eq (sbw/ht-map-vals inc-val hash-table) expected))))

;; sbw/ht-update

(ert-deftest sbw/ht-update-given-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k 1))
          (inc        (lambda (x) (+ 1 x)))
          (expected   (sbw/ht-create :k 2)) )
    (should (sbw/value-eq (sbw/ht-update hash-table :k inc) expected))))

(ert-deftest sbw/ht-update-given-new-key-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-update hash-table :k2 f) expected))))

;; sbw/ht-assoc-in

(ert-deftest sbw/ht-assoc-in-given-root-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (expected   (sbw/ht-create :k1 :v2)) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k1] :v2) expected))))

(ert-deftest sbw/ht-assoc-in-given-root-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (expected   (sbw/ht-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k2] :v2) expected))))

(ert-deftest sbw/ht-assoc-in-given-nested-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v1)))
          (expected   (sbw/ht-create :k1 (sbw/ht-create :k2 :v2))) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k1 :k2] :v2) expected))))

(ert-deftest sbw/ht-assoc-in-given-nested-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v1)))
          (expected   (sbw/ht-create :k1 (sbw/ht-create :k2 :v1 :k3 :v2))) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k1 :k3] :v2) expected))))

(ert-deftest sbw/ht-assoc-in-given-path-through-existing-non-nested-structure-then-throws ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1)) )
    (should-error (sbw/ht-assoc-in hash-table [:k1 :k2] :v1) :type 'wrong-type-argument)))

(ert-deftest sbw/ht-assoc-in-given-path-through-non-existent-key-then-adds-empty-hash-table ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (expected   (sbw/ht-create :k1 :v1 :k2 (sbw/ht-create :k3 (sbw/ht-create :k4 :v2)))) )
    (should (sbw/value-eq (sbw/ht-assoc-in hash-table [:k2 :k3 :k4] :v2) expected))))

;; sbw/ht-update-in

(ert-deftest sbw/ht-update-in-given-root-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (f          (lambda (x) (when (equal x :v1) :v2)))
          (expected   (sbw/ht-create :k1 :v2)) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k1] f) expected))))

(ert-deftest sbw/ht-update-in-given-root-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht-create :k1 :v1 :k2 :v2)) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k2] f) expected))))

(ert-deftest sbw/ht-update-in-given-nested-key-exists-then-replaces ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v1)))
          (f          (lambda (x) (when (equal x :v1) :v2)))
          (expected   (sbw/ht-create :k1 (sbw/ht-create :k2 :v2))) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k1 :k2] f) expected))))

(ert-deftest sbw/ht-update-in-given-nested-key-does-not-exist-then-adds ()
  (let* ( (hash-table (sbw/ht-create :k1 (sbw/ht-create :k2 :v1)))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht-create :k1 (sbw/ht-create :k2 :v1 :k3 :v2))) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k1 :k3] f) expected))))

(ert-deftest sbw/ht-update-in-given-path-through-existing-non-nested-structure-then-throws ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1)) )
    (should-error (sbw/ht-update-in hash-table [:k1 :k2] :v1) :type 'wrong-type-argument)))

(ert-deftest sbw/ht-update-in-given-path-through-non-existent-key-then-adds-empty-hash-table ()
  (let* ( (hash-table (sbw/ht-create :k1 :v1))
          (f          (lambda (x) (when (not x) :v2)))
          (expected   (sbw/ht-create :k1 :v1 :k2 (sbw/ht-create :k3 (sbw/ht-create :k4 :v2)))) )
    (should (sbw/value-eq (sbw/ht-update-in hash-table [:k2 :k3 :k4] f) expected))))

(provide 'sbw-hash-tables-test)
