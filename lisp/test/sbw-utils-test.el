(require 'sbw-utils)
(require 'sbw-hash-tables)

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

;; sbw/decompose-time

;; TODO

;; sbw/compose-time

;; TODO

(provide 'sbw-utils-test)
