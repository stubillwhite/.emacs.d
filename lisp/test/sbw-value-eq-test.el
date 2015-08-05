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
    `(,(sbw/ht-create)                 ,(sbw/ht-create)                         t)
    `(,(sbw/ht-create :foo 23 :bar 42) ,(sbw/ht-create :foo 23 :bar 42)         t)
    `(,(sbw/ht-create :foo 23 :bar 42) ,(sbw/ht-create :foo 42 :bar 23)         nil)
    `(,(sbw/ht-create :foo 23 :bar 42) ,(sbw/ht-create :foo 23 :bar 42 :baz 13) nil)
    `(,(sbw/ht-create)                 nil                                      nil)
    `(,(sbw/ht-create)                 ,(list)                                  nil)

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
    
    ;; Mixed
    
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

(ert-deftest sbw/value-eq-given-mixed-type-structures-then-true-if-equal ()
  (let* ( (equal-one (sbw/ht-create (sbw/ht-create :foo 23) (list 1 2 3 "4")))
          (equal-two (sbw/ht-create (sbw/ht-create :foo 23) (list 1 2 3 "4")))
          (different (sbw/ht-create (sbw/ht-create :bar 23) (list 1 2 3 "4"))) )
    (should (eq (sbw/value-eq equal-one equal-two) t))
    (should (eq (sbw/value-eq equal-one different) nil))
    (should (eq (sbw/value-eq equal-two different) nil))))



