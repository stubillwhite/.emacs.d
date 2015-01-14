(require 'sbw-time)
(require 'sbw-hash-tables)
(require 'dash)

;; sbw/time-decompose

(ert-deftest sbw/time-decompose-then-returns-decomposed-time ()
  "sbw/time-decompose returns decomposed time"
  (let* ( (epoch-time (seconds-to-time 0))
          (expected   (sbw/ht-create :second 0 :minute 0 :hour 1 :day 1 :month 1 :year 1970 :weekday 4 :daylight-saving nil :timezone 3600)))
    (should (sbw/ht-equal (sbw/time-decompose epoch-time) expected))))

;; sbw/time-compose

(ert-deftest sbw/time-compose-then-returns-composed-time ()
  "sbw/time-compose returns composed time"
  (let* ( (epoch-decomposed (sbw/ht-create :second 0 :minute 0 :hour 1 :day 1 :month 1 :year 1970 :weekday 4 :daylight-saving nil :timezone 3600))
          (expected         (seconds-to-time 0)) )
    (should (equal (time-to-seconds (sbw/time-compose epoch-decomposed)) (time-to-seconds expected)))))

;; sbw/time-as-date-only

(ert-deftest sbw/time-as-date-only-needs-implementing ()
  :expected-result :failed
  "TODO description"
  (should (equal t nil)))

;; sbw/time-as-time-only

(ert-deftest sbw/time-as-time-only-needs-implementing ()
  :expected-result :failed
  "TODO description"
  (should (equal t nil)))

;; sbw/time-adjust-by

(ert-deftest sbw/time-adjust-by-needs-implementing ()
  :expected-result :failed
  "TODO description"
  (should (equal t nil)))

;; sbw/time-max

(ert-deftest sbw/time-max-needs-implementing ()
  :expected-result :failed
  "TODO description"
  (should (equal t nil)))

;; sbw/time-min

(ert-deftest sbw/time-min-needs-implementing ()
  :expected-result :failed
  "TODO description"
  (should (equal t nil)))
