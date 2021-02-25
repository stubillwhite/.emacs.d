(setq sbw/value-eq--test--ht
  (let* ( (ht (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 23 ht)
    (puthash :bar 42 ht)
    ht))

(setq sbw/value-eq--test--ht-same-one
  (let* ( (ht (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 23 ht)
    (puthash :bar 42 ht)
    ht))

(setq sbw/value-eq--test--ht-same-two
  (let* ( (ht (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :bar 42 ht)
    (puthash :foo 23 ht)   
    ht))

(setq sbw/value-eq--test--ht-different-one
  (let* ( (ht (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 42 ht)
    (puthash :bar 23 ht)   
    ht))

(setq sbw/value-eq--test--ht-different-two
  (let* ( (ht (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 23 ht)
    (puthash :bar 42 ht)
    (puthash :baz 50 ht)
    ht))

(setq sbw/value-eq--test--test-data
  ;; Test data of the form [value-one value-two are-equal?]
  (list
    ;; nil
    `(nil nil t)
    
    ;; String
    `("foo" "foo" t)
    `("foo" "bar" nil)
    `("foo" nil   nil)

    ;; Keyword
    `(:foo :foo t)
    `(:foo :bar nil)
    `(:foo nil  nil)
    `(:foo 23   nil)

    ;; Number
    `(23 23               t)
    `(23 42               nil)
    `(23 nil              nil)
    `(23 ,(sbw/ht-create) nil)

    ;; Hash table
    `(,(make-hash-table)      ,(make-hash-table)                    t)
    `(,sbw/value-eq--test--ht ,sbw/value-eq--test--ht               t)
    `(,sbw/value-eq--test--ht ,sbw/value-eq--test--ht-same-one      t)
    `(,sbw/value-eq--test--ht ,sbw/value-eq--test--ht-same-two      t)
    `(,sbw/value-eq--test--ht ,sbw/value-eq--test--ht-different-one nil)
    `(,sbw/value-eq--test--ht ,sbw/value-eq--test--ht-different-two nil)
    `(,(make-hash-table)      nil                                   nil)
    `(,(make-hash-table)      ,(list)                               nil)

    ;; List
    `(,(list)    ,(list)       t)
    `(,(list 23) ,(list 23)    t)
    `(,(list 23) ,(list 42)    nil)
    `(,(list 23) ,(list 23 42) nil)
    `(,(list 23) nil           nil)
    `(,(list 23) ,(vector 23)  nil)

    ;; Vector
    `(,(vector)    ,(vector)       t)
    `(,(vector 23) ,(vector 23)    t)
    `(,(vector 23) ,(vector 42)    nil)
    `(,(vector 23) ,(vector 23 42) nil)
    `(,(vector 23) nil             nil)
    `(,(vector 23) "23"            nil)    
    ))

(ert-deftest sbw/value-eq-data-driven-test ()
  (-each sbw/value-eq--test--test-data
    (lambda (data)
      (let* ( (a          (car   data))
              (b          (cadr  data))
              (are-equal? (caddr data)) )
        (if are-equal?
          (should     (sbw/value-eq a b))
          (should-not (sbw/value-eq a b))))))
  t)

(defun sbw/value-eq--test--hashcode-equal (a b)
  (eq (sbw/value-eq-hashcode a) (sbw/value-eq-hashcode b)))

(ert-deftest sbw/value-eq-hashcode-data-driven-test ()
  (-each sbw/value-eq--test--test-data
    (lambda (data)
      (let* ( (a          (car   data))
              (b          (cadr  data))
              (are-equal? (caddr data)) )
        (if are-equal?
          (should     (sbw/value-eq--test--hashcode-equal a b))
          (should-not (sbw/value-eq--test--hashcode-equal a b))))))
  t)

;; { { :foo 23 } (1 2 3 "4") }
(setq sbw/value-eq--test--mixed-struct
  (let* ( (ht-outer (make-hash-table :test 'sbw/value-eq-test))
          (ht-inner (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 23 ht-inner)
    (puthash ht-inner (list 1 2 3 "4") ht-outer)
    ht-outer))

;; { { :foo 23 } (1 2 3 "4") }
(setq sbw/value-eq--test--mixed-struct-same
  (let* ( (ht-outer (make-hash-table :test 'sbw/value-eq-test))
          (ht-inner (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :foo 23 ht-inner)
    (puthash ht-inner (list 1 2 3 "4") ht-outer)
    ht-outer))

;; { { :bar 23 } (1 2 3 "4") }
(setq sbw/value-eq--test--mixed-struct-different
  (let* ( (ht-outer (make-hash-table :test 'sbw/value-eq-test))
          (ht-inner (make-hash-table :test 'sbw/value-eq-test)) )
    (puthash :bar 23 ht-inner)
    (puthash ht-inner (list 1 2 3 "4") ht-outer)
    ht-outer))

(ert-deftest sbw/value-eq-given-mixed-type-structures-then-true-if-equal ()
  (let* ( (struct    sbw/value-eq--test--mixed-struct)
          (same      sbw/value-eq--test--mixed-struct-same)
          (different sbw/value-eq--test--mixed-struct-different))
    (should (eq (sbw/value-eq struct struct)    t))
    (should (eq (sbw/value-eq struct same)      t))
    (should (eq (sbw/value-eq struct different) nil))))

(provide 'sbw-value-eq-test)
