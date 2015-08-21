(require 'dash)
(require 'subr-x)

(defun sbw/ht2-create--internal (comp keyvals)
  (let* ( (even? (lambda (x) (= 0 (mod x 2)))) )
    (when (not (funcall even? (length keyvals)))
      (signal 'wrong-number-of-arguments keyvals))
    (-reduce-from
      (lambda (acc kv) (puthash (car kv) (cadr kv) acc) acc)
      (make-hash-table :test comp)
      (-partition 2 keyvals))))

(defun sbw/ht2-create-with-comparator (comp &rest keyvals)
  "Returns a new hash-table with comparator COMP, and the initial
content specified by KEYVALS key and value sequence."
  (sbw/ht2-create--internal comp keyvals))

(defun sbw/ht2-create (&rest keyvals)
  "Returns a new hash-table with an equal comparator, and the
initial content specified by KEYVALS key and value sequence."
  (sbw/ht2-create--internal 'sbw/value-eq-test keyvals))

(defun sbw/ht2-keys (hash-table)
  "Returns the keys from HASH-TABLE."
  (hash-table-keys hash-table))

(defun sbw/ht2-vals (hash-table)
  "Returns the values from HASH-TABLE."
  (hash-table-values hash-table))

(defun sbw/ht2-get (hash-table k &optional default)
  "Returns the value associated with key K in HASH-TABLE, nil or
DEFAULT if no such key exists."
  (gethash k hash-table default))

(defun sbw/ht2-get-in (hash-table ks &optional default)
  "Returns the value from the associative structure at the point
specified by KS, where KS is a sequence of keys into the
structure."
  (seq-reduce
    (lambda (acc x)
      (cond
        ((hash-table-p acc) (sbw/ht2-get acc x default))
        (t                  (signal 'wrong-type-argument acc))))
    ks
    hash-table))

(defun sbw/ht2-contains? (hash-table k)
  "Returns t if HASH-TABLE contains key K, nil otherwise."
  (-contains? (sbw/ht2-keys hash-table) k))

(defun sbw/ht2-copy (hash-table)
  "Returns a copy of HASH-TABLE."
  (copy-hash-table hash-table))

(defun sbw/ht2-merge (&rest hash-tables)
  "Returns a new hash-table all the hash-tables merged together.
If a key occurs in more than one hash-table then the
latter (left-to-right) will be the mapping in the result."
  (-reduce-from
    (lambda (acc ht) (maphash (lambda (k v) (puthash k v acc)) ht) acc)
    (sbw/ht2-create)
    hash-tables))

(defun sbw/ht2-assoc (hash-table k v)
  "Returns a copy of HASH-TABLE with K mapped to V."
  (sbw/ht2-merge hash-table (sbw/ht2-create k v)))

(defun sbw/ht2-dissoc (hash-table k)
  "Returns a copy of HASH-TABLE with K removed."
  (let* ( (copy (sbw/ht2-copy hash-table)) )
    (remhash k copy)
    copy))

(defun sbw/ht2-select-keys (hash-table ks)
  "Returns a copy of HASH-TABLE with only the values associated
with the specified keys."
  (-reduce-from
    (lambda (acc k)
      (let* ( (v (gethash k hash-table :sbw/key-not-found)) )
        (when (not (equal v :sbw/key-not-found))
          (puthash k (gethash k hash-table) acc))
        acc))
    (sbw/ht2-create)
    (append ks nil)))

(defun sbw/ht2-zipmap (ks vals)
  "Returns a new HASH-TABLE with keys KS mapped to VALS."
  (-reduce-from
    (lambda (acc v) (sbw/ht2-assoc acc (car v) (cadr v)))
    (sbw/ht2-create)
    (-partition 2 (-interleave ks vals))))

(defun sbw/ht2-map-vals (f hash-table)
  "Returns a copy of HASH-TABLE with f applied to each value."
  (-let* ( (keys (sbw/ht2-keys hash-table)) )
    (apply 'sbw/ht2-create (-interleave keys (-map (lambda (x) (funcall f (sbw/ht2-get hash-table x))) keys)))))

(defun sbw/ht2-update (hash-table k f)
  "Returns a copy of HASH-TABLE with the value associated with
key K updated, where f is a function that will take the old value
and return the new value."
  (let* ( (v (funcall f (sbw/ht2-get hash-table k))) )
    (sbw/ht2-merge hash-table (sbw/ht2-create k v))))

(defun sbw/ht2-update-in (hash-table ks f)
  "Returns a nested associative structure with value associated
at the point specified by KS updated, where KS is a sequence of
keys to the structure, and F is function that will take the old
value and return the new value. If any levels do not exist then
new hash-tables will be created."
  (let* ( (k-seq (seq-into ks 'list))
          (k     (car k-seq))
          (ks    (cdr k-seq)) )
    (if (seq-empty-p ks)
      (sbw/ht2-assoc hash-table k (funcall f (sbw/ht2-get hash-table k)))
      (sbw/ht2-assoc hash-table k (sbw/ht2-update-in (sbw/ht2-get hash-table k (sbw/ht2-create)) ks f)))))

(defun sbw/ht2-assoc-in (hash-table ks v)
  "Returns a nested associative structure with value V associated
at the point specified by KS, where KS is a sequence of keys to
the structure. If any levels do not exist then new hash-tables
will be created."
  (sbw/ht2-update-in hash-table ks (lambda (x) v)))

(provide 'sbw-hash-tables-two)
