(require 'sbw-org-review)

(defun file-content (filename)
  "Returns the content of filename as a string."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun sbw/-org-review-test-org-files ()
  (-map
    (lambda (x) (concat "~/.emacs.d/lisp/test/org-review" x))
    (list "/test-file-one.org" "/test-file-two.org")))

(ert-deftest sbw/org-review-generate-category-report-then-expected-report ()
  (should (string=
            (sbw/org-review-generate-category-report (sbw/-org-review-test-org-files) 'sbw/-org-review-default-formatter-func)
            (file-content "expected.txt"))))

(provide 'sbw-org-reports-test)
