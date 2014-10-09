;; Functions for hash-table manipulation, heavily stolen from Clojure's API

(require 'dash)

(defun sbw/ht-keys (hash-table)
  "Returns the keys from HASH-TABLE."
  (let* ( (keys ()) )
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys))

(defun sbw/ht-vals (hash-table)
  "Returns the values from HASH-TABLE."
  (let* ( (vals ()) )
    (maphash (lambda (k v) (push v vals)) hash-table)
    vals))

(defun sbw/ht-equal (a b)
  "Returns t if hash-tables A and B are equal, nil otherwise."
  (let* ( (a    (copy-hash-table a))
          (b    (copy-hash-table b))
          (keys (sbw/ht-keys a)) )
    (while keys
      (let ( (k (car keys)) )
        (when (equal (gethash k a) (gethash k b :sbw/key-not-found))
          (remhash k a)
          (remhash k b))
        (setq keys (cdr keys))))
    (and
      (not (sbw/ht-keys a))
      (not (sbw/ht-keys b)))))

(defun sbw/ht-create (&rest keyvals)
  "Returns a new hash-table with an equal comparator, and the initial content specified by KEYVALS key and value sequence."
  (let* ( (even? (lambda (x) (= 0 (mod x 2)))) )
    (when (not (funcall even? (length keyvals)))
      (signal 'wrong-number-of-arguments keyvals))
    (-reduce-from
      (lambda (acc kv) (puthash (car kv) (cadr kv) acc) acc)
      (make-hash-table :test 'equal)
      (-partition 2 keyvals))))

(defun sbw/ht-get (hash-table k)
  "Returns the value associated with key K in HASH-TABLE or nil if no such key exists."
  (gethash k hash-table))

(defun sbw/ht-contains? (hash-table k)
  "Returns t if HASH-TABLE contains key K, nil otherwise."
  (-contains? (sbw/ht-keys hash-table) k))

(defun sbw/ht-copy (hash-table)
  "Returns a copy of HASH-TABLE."
  (copy-hash-table hash-table))

(defun sbw/ht-merge (&rest hash-tables)
  "Returns a new hash-table all the hash-tables merged together. If a key occurs in more than one hash-table then the latter (left-to-right) will be the mapping in the result."
  (-reduce-from
    (lambda (acc ht) (maphash (lambda (k v) (puthash k v acc)) ht) acc)
    (sbw/ht-create)
    hash-tables))

(defun sbw/ht-assoc (hash-table k v)
  "Returns a copy of HASH-TABLE with K mapped to V."
  (sbw/ht-merge hash-table (sbw/ht-create k v)))

(defun sbw/ht-assoc-in (hash-table ks v)
  "TODO"
  nil)

;; TODO
;; sbw/ht-assoc-in map ks v
;; sbw/ht-dissoc map k
;; sbw/select-keys map ks
;; sbw/ht-update map k f
;; sbw/ht-update-in map ks f

(provide 'sbw-hash-table-utils)
