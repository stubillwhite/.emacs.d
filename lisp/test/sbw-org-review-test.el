(require 'sbw-org-review)

(lexical-let* ( (start     (days-to-time 0))
                (end       (days-to-time 7))
                (test-file (lambda (x) (format "~/.emacs.d/lisp/test/org-review-test/%s" x)))
                (config    (sbw/org-review-config nil nil start end nil))
                (file-text (lambda (x) (f-read-text (funcall test-file x)))) )

  (ert-deftest sbw/org-review-completed-tasks-generate-report-then-expected-report ()
    "sbw/org-review-completed-tasks-generate-report then expected report"
    (lexical-let* ( (summaries (sbw/org-review-heading-summaries-for-file (funcall test-file "completed-tasks-report-input.org")))
                    (expected  (funcall file-text "completed-tasks-report-expected.txt"))
                    (actual    (sbw/org-review-completed-tasks-generate-report config summaries)) )
      (should (string= actual expected))))
  )

(provide 'sbw-org-reports-test)
