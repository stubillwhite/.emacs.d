(require 'sbw-multimethods)

(ert-deftest sbw/mmultimethod-invocation-then-dispatches-correctly ()
  (progn
    (sbw/mm-defmulti describe-number (lambda (x) (if (= 0 (mod x 2)) :even :odd)))
    (sbw/mm-defmethod describe-number [:even] (x) :number-is-even)
    (sbw/mm-defmethod describe-number [:odd]  (x) :number-is-odd)
    (should (eq :number-is-even (describe-number 22)))
    (should (eq :number-is-odd  (describe-number 23)))
    (sbw/mm-undefmulti describe-number)))
