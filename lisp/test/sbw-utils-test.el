(require 'sbw-utils)
(require 'sbw-hash-tables)

;; sbw/assq-ensure-is-first

(ert-deftest sbw/assq-ensure-is-first-given-map-then-removes-duplicates-and-ensures-is-first ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'b (list a-entry b-entry c-entry)) (list b-entry a-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'c (list a-entry b-entry c-entry)) (list c-entry a-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry a-entry)) (list a-entry b-entry)))))

;; sbw/assq-ensure-is-last

(ert-deftest sbw/assq-ensure-is-last-given-map-then-removes-duplicates-and-ensures-is-last ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-last 'a (list a-entry b-entry c-entry)) (list b-entry c-entry a-entry)))
    (should (equal (sbw/assq-ensure-is-last 'b (list a-entry b-entry c-entry)) (list a-entry c-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-last 'c (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-last 'a (list a-entry b-entry a-entry)) (list b-entry a-entry)))))

;; sbw/truncate-string

(ert-deftest sbw/truncate-string-given-string-shorter-then-returns-string-unchanged ()
  (let* ( (s "This is a test") )
    (should (equal (sbw/truncate-string s 14) "This is a test"))))

(ert-deftest sbw/truncate-string-given-string-longer-then-returns-string-truncated ()
  (let* ( (s "This is a test") )
    (should (equal (sbw/truncate-string s 13) (concat "This is a te" "\u2026")))))

(provide 'sbw-utils-test)
