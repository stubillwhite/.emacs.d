(require 'sbw-multimethods)

(defmacro sbw/mm--test--with-test-registry (&rest body)
  `(lexical-let* ( (*sbw/mm--registry* (sbw/mm--create-hash-table)) )
     ,@body))

(ert-deftest sbw/mm-invocation-given-match-then-dispatches ()
  (sbw/mm--test--with-test-registry
    (progn
      (sbw/mm-defmulti describe-number (lambda (x) (if (= 0 (mod x 2)) :even :odd)))
      (sbw/mm-defmethod describe-number [:even] (x) :number-is-even)
      (sbw/mm-defmethod describe-number [:odd]  (x) :number-is-odd)
      (should (eq :number-is-even (describe-number 22)))
      (should (eq :number-is-odd  (describe-number 23))))))

(ert-deftest sbw/mm-invocation-given-value-does-not-match-then-error ()
  (sbw/mm--test--with-test-registry
    (progn
      (sbw/mm-defmulti a-multimethod (lambda (x) x))
      (sbw/mm-defmethod a-multimethod [:foo] (x) :foo-invoked)
      (should (eq :foo-invoked (a-multimethod :foo)))
      (should-error (a-multimethod :bar) :type 'wrong-type-argument))))

(ert-deftest sbw/mm-invocation-given-method-deleted-then-error ()
  (sbw/mm--test--with-test-registry
    (progn
      (sbw/mm-defmulti a-multimethod (lambda (x) x))
      (sbw/mm-defmethod a-multimethod [:foo] (x) :foo-invoked)
      (should (eq :foo-invoked (a-multimethod :foo)))
      (sbw/mm-undefmulti a-multimethod)
      (should-error (a-multimethod :foo) :type 'wrong-type-argument))))
