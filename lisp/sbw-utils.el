(defun sbw/filter (p l)
  "Returns the items from list L for which predicate P is true."
  (delq nil
        (mapcar (lambda (x) (and (funcall p x) x)) l)))

(defun sbw/assq-ensure-is-first (key alist)
  "Mutates associative ALIST so that the value for KEY is first."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry)))

(defun sbw/assq-ensure-is-last (key alist)
  "Mutates associative ALIST so that the value for KEY is last."
  (let ((entry (assq key alist)))
    (setq alist (assq-delete-all key alist))
    (add-to-list 'alist entry :append)))

(defun sbw/heading-one (s)
  "Returns string S formatted to be a top level heading."
  (concat s "\n" (make-string (length s) ?=) "\n"))

(defun sbw/heading-two (s)
  "Returns string S formatted to be a sub-heading."
  (concat s "\n" (make-string (length s) ?-) "\n"))

(defun sbw/truncate-string (s n)
  "Returns S truncated to N characters with ellipsis if truncation occurred."
  (if (> (length s) n)
    (concat (substring s 0 (- n 1)) "\u2026")
    s))

(defun sbw/map-hash (f hash-table)
  "Returns a list of the result of calling F on each key value entry in the specified HASH-TABLE."
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
(defun sbw/adjust-time-by-days (time n)
  "Returns TIME adjusted by N days."
  (days-to-time (+ (time-to-number-of-days time) n)))

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
