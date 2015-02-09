(require 'sbw-org-review)

;; TODO Find a better way to get test file paths
(lexical-let* ( (start     (days-to-time 0))
                (end       (days-to-time 7))
                (test-file (lambda (x) (format "~/.emacs.d/lisp/test/org-review-test/%s" x)))
                (summaries (sbw/org-review-heading-summaries-for-file (funcall test-file "clocked-time-report-input.org")))
                (config    (sbw/org-review-config nil nil start end nil))
                (file-text (lambda (x) (f-read-text (funcall test-file x)))) )

  (ert-deftest sbw/org-review-clocked-time-report-generate-then-expected-report ()
    "sbw/org-review-clocked-time-report-generate then expected report."
    (lexical-let* ( (expected (funcall file-text "clocked-time-report-expected.txt"))
                    (actual   (sbw/org-review-clocked-time-report-generate config summaries)) )
      (should (string= actual expected))))
  
  )






(provide 'sbw-org-reports-test)
