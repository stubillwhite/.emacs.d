(require 'sbw-org-utils)

(lexical-let* ( (start    (days-to-time 0))
                (end      (days-to-time 7))
                (filename "/Users/white1/.emacs.d/lisp/test/org-utils-test/input.org")
                (expected-summary-1 (sbw/ht-create :filename filename
                                                   :point    1
                                                   :category nil
                                                   :state    nil
                                                   :tags     nil
                                                   :heading  "Title"
                                                   :level    1
                                                   :clock    nil
                                                   :closed   nil))
                (expected-summary-2 (sbw/ht-create :filename filename
                                                   :point    9
                                                   :category nil
                                                   :state    "TODO"
                                                   :tags     nil
                                                   :heading  "Task one"
                                                   :level    2
                                                   :clock    (list
                                                              "[2016-09-24 Sat 13:54]--[2016-09-25 Sun 13:54]"
                                                              "[2016-09-23 Fri 13:54]--[2016-09-24 Sat 07:54]")
                                                   :closed   nil)) )

  (ert-deftest sbw/org-utils-heading-summaries-for-file-then-expected-summaries ()
    "sbw/org-utils-heading-summaries-for-file then expected summaries"
    (lexical-let* ( (actual   (sbw/org-utils-heading-summaries-for-file filename))
                    (expected (list expected-summary-1
                                    expected-summary-2)))
      (should (sbw/value-eq actual expected))))

  ;; (pp (sbw/org-utils-heading-summaries-for-file filename)) 
  ;; (sbw/pprint-as-json (sbw/org-utils-heading-summaries-for-file filename))
  nil)

;; Tests for sbw/org-utils--replace-urls-with-descriptions

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-file-path-and-description ()
  "sbw/org-utils--replace-urls-with-descriptions given file+sys link with description then description"
  (should (equal "pretext (description)"
                 (sbw/org-utils--replace-urls-with-descriptions "pretext ([[file+sys:path\\ with\\ spaces][description]])"))))

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-file-path-no-description ()
  "sbw/org-utils--replace-urls-with-descriptions given file+sys link without description then URL"
  (should (equal "pretext (file+sys:just-url)"
                 (sbw/org-utils--replace-urls-with-descriptions "pretext ([[file+sys:just-url]])"))))

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-https-and-description ()
  "sbw/org-utils--replace-urls-with-descriptions given https link with description then description"
  (should (equal "pretext (description)"
                 (sbw/org-utils--replace-urls-with-descriptions "pretext ([[https://url][description]])"))))

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-multiple-links ()
  "sbw/org-utils--replace-urls-with-descriptions given multiple links the processes all"
  (should (equal "pretext (link one description and link two description)"
                 (sbw/org-utils--replace-urls-with-descriptions "pretext ([[file+sys:/Books/path\\ with\\ spaces][link one description]] and [[https://another/url][link two description]])"))))

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-no-links ()
  "sbw/org-utils--replace-urls-with-descriptions given no links then unchanged"
  (should (equal "plain text with no links"
                 (sbw/org-utils--replace-urls-with-descriptions "plain text with no links"))))

(ert-deftest sbw/org-utils--replace-urls-with-descriptions-with-backslashes-in-description ()
  "sbw/org-utils--replace-urls-with-descriptions given link with backslash literals in description then retains backslashes"
  (should (equal "pretext link \\\\ description"
                 (sbw/org-utils--replace-urls-with-descriptions "pretext [[https://another/url][link \\\\ description]]"))))

(provide 'sbw-org-utils-test)
