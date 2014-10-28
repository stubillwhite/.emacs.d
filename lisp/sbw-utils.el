;; TODO: Remove, use -filter?
(defun sbw/filter (p l)
  "Returns the items from list l for which predicate p is true."
  (delq nil
        (mapcar (lambda (x) (and (funcall p x) x)) l)))

(ert-deftest sbw/filter-given-predicate-and-list-then-retains-items-for-which-predicate-is-true ()
  "sbw/filter given predicate and list then retains items for which predicate is true."
  (let ((is-even? (lambda (x) (= (% x 2) 0))))
    (should (equal (sbw/filter is-even? (list))         (list)))
    (should (equal (sbw/filter is-even? (list 1 3 5 7)) (list)))
    (should (equal (sbw/filter is-even? (list 1 2 3 4)) (list 2 4)))
    (should (equal (sbw/filter is-even? (list 2 4 6 8)) (list 2 4 6 8)))))

(defun sbw/assq-ensure-is-first (key alist)
  "Mutates associative alist so that the value for key is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(ert-deftest sbw/assq-ensure-is-first-given-map-then-removes-duplicates-and-ensures-is-first ()
  (let* ( (a-entry (list 'a "a-entry"))
          (b-entry (list 'b "b-entry"))
          (c-entry (list 'c "c-entry")) )
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry c-entry)) (list a-entry b-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'b (list a-entry b-entry c-entry)) (list b-entry a-entry c-entry)))
    (should (equal (sbw/assq-ensure-is-first 'c (list a-entry b-entry c-entry)) (list c-entry a-entry b-entry)))
    (should (equal (sbw/assq-ensure-is-first 'a (list a-entry b-entry a-entry)) (list a-entry b-entry)))))

;; TODO - tested

(defun sbw/heading-one (s)
  "Returns string S formatted to be a top level heading."
  (concat s "\n" (make-string (length s) ?=) "\n"))

(defun sbw/heading-two (s)
  "Returns string S formatted to be a sub-heading."
  (concat s "\n" (make-string (length s) ?-) "\n"))

(defun sbw/truncate-string (s n)
  "Returns s truncated to n characters with ellipsis if truncation occurred."
  (if (> (length s) n)
    (concat (substring s 0 (- n (length org-ellipsis))) org-ellipsis)
    s))

(defun sbw/map-hash (f hash-table)
  "Returns a list of the result of calling f on each key value entry in the specified hash-table."
  (let ( (results (list)) )
    (maphash (lambda (k v) (push (funcall f k v) results)) hash-table)
    results))

(defun sbw/decompose-time (time)
  "Returns TIME in the form of a hash-table."
  (apply 'sbw/ht-create
    (apply '-concat
      (-zip-with
        'list
        (list :second :minute :hour :day :month :year :weekday :daylight-saving :timezone)
        (decode-time time)))))

(defun sbw/compose-time (decomposed-time)
  "Returns the time represented by the DECOMPOSED-TIME hash-table."
  (apply 'encode-time
    (-reduce-from
      (lambda (acc x) (cons (gethash x decomposed-time) acc))
      '()
      (list :timezone :daylight-saving :weekday :year :month :day :hour :minute :second))))

;; TODO Test this
(defun sbw/collect-by (f l)
  "Returns a hash-table of lists of items, keyed by the result of (f item) for each item in list l."
  (-reduce-from    
    (lambda (acc val)
      (let* ( (category (funcall f val))
              (curr-val (gethash category acc (list))) )
        (puthash category (cons val curr-val) acc)
        acc))
    (sbw/ht-create)
    l))

;; TODO Might be replaced with s.el now?
;; TODO Test these, break tests into a separate file
(defun sbw/join (sep list)
  "Returns a string with the items from list joined by separator."
  (apply 'concat (-drop 1 (-interleave (-repeat (length list) sep) list))))

;; TODO Prefix these
(defalias 'sbw/dec '1- "Return x minus one.")
(defalias 'sbw/inc '1+ "Return x plus one.")

(provide 'sbw-utils)