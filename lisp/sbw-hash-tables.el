;; Functions for hash-table manipulation, heavily stolen from Clojure's API

(require 'dash)
(require 'subr-x)

(defun sbw/ht-keys (hash-table)
  "Returns the keys from HASH-TABLE."
  (hash-table-keys hash-table))

(defun sbw/ht-vals (hash-table)
  "Returns the values from HASH-TABLE."
  (hash-table-values hash-table))

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

(defun sbw/ht-get (hash-table k &optional default)
  "Returns the value associated with key K in HASH-TABLE, nil or DEFAULT if no such key exists."
  (gethash k hash-table default))

(defun sbw/ht-get-in (hash-table ks &optional default)
  "Returns the value from the associative structure at the point specified by KS, where KS is a sequence of keys into the structure."
  (let* ( (result (-reduce-from
                    (lambda (acc k)
                      (if (equal acc :sbw/key-not-found)
                        acc
                        (sbw/ht-get acc k :sbw/key-not-found)))
                    hash-table
                    (append ks nil))) )
    (if (equal result :sbw/key-not-found)
      default
      result)))

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

(defun sbw/-ht-flatten-hash-tables (hash-table ks)
  "Returns a flattened version of a nested associative structure to the point specified by KS, where KS is a sequence of keys to the structure. If any levels do not exist then new hash-tables will be created."
  (let* ( (mk-acc        (lambda (ht l) (sbw/ht-create :ht ht :l l)))
          (get-or-create (lambda (ht k) (let ((x (sbw/ht-get ht k))) (if x x (sbw/ht-create))))) )
    (sbw/ht-get
      (-reduce-from
        (lambda (acc k)
          (let* ( (ht     (sbw/ht-get acc :ht))
                  (l      (sbw/ht-get acc :l))
                  (new-ht (sbw/ht-get ht k)) )
            (funcall mk-acc
              (funcall get-or-create ht k)
              (append l (list ht)))))
        (funcall mk-acc hash-table (list))
        (append ks nil))
      :l)))

(defun sbw/ht-assoc-in (hash-table ks v)
  "Returns a nested associative structure with value V associated at the point specified by KS, where KS is a sequence of keys to the structure. If any levels do not exist then new hash-tables will be created."
  (-reduce-from
    (lambda (acc ht-and-k)
      (let ( (ht (car ht-and-k))
             (k  (cadr ht-and-k)) )
        (sbw/ht-assoc ht k acc)))
    v
    (reverse (-partition 2 (-interleave (sbw/-ht-flatten-hash-tables hash-table ks) (append ks nil))))))

(defun sbw/ht-update (hash-table k f)
  "Returns a copy of HASH-TABLE with the value associated with key K updated, where f is a function that will take the old value and return the new value."
  (let* ( (v (funcall f (sbw/ht-get hash-table k))) )
    (sbw/ht-merge hash-table (sbw/ht-create k v))))


;; TODO Create drill-into function that reduces a hash-map into the values retrieved from the map
;;      update-in then does zipmap on the keys and that stream
(defun sbw/ht-update-in (hash-table ks f)
  "Returns a nested associative structure with value associated at the point specified by KS updated, where KS is a sequence of keys to the structure, and F is function that will take the old value and return the new value. If any levels do not exist then new hash-tables will be created."
  (let* ( (flattened (sbw/-ht-flatten-hash-tables hash-table ks)) )
    (-reduce-from
      (lambda (acc ht-and-k)
        (let ( (ht (car ht-and-k))
               (k  (cadr ht-and-k)) )
          (sbw/ht-assoc ht k acc)))
      (funcall f (car flattened))
      (reverse (-partition 2 (-interleave flattened (append ks nil)))))))

(defun sbw/ht-dissoc (hash-table k)
  "Returns a copy of HASH-TABLE with K removed."
  (let* ( (copy (sbw/ht-copy hash-table)) )
    (remhash k copy)
    copy))

(defun sbw/ht-select-keys (hash-table ks)
  "Returns a copy of HASH-TABLE with only the values associated with the specified keys."
  (-reduce-from
    (lambda (acc k)
      (let* ( (v (gethash k hash-table :sbw/key-not-found)) )
        (when (not (equal v :sbw/key-not-found))
          (puthash k (gethash k hash-table) acc))
        acc))
    (sbw/ht-create)
    (append ks nil)))

;; TODO: This is the wrong way around; should be (f hash-table)
(defun sbw/ht-map-vals (hash-table f)
  "Returns a copy of HASH-TABLE with f applied to each value."
  (-let* ( (keys (sbw/ht-keys hash-table)) )
    (apply 'sbw/ht-create (-interleave keys (-map (lambda (x) (funcall f (sbw/ht-get hash-table x))) keys)))))

(defun sbw/ht-zipmap (ks vals)
  "Returns a new HASH-TABLE with keys KS mapped to VALS."
  (-reduce-from
    (lambda (acc v) (sbw/ht-assoc acc (car v) (cadr v)))
    (sbw/ht-create)
    (-partition 2 (-interleave ks vals))))

(provide 'sbw-hash-tables)
