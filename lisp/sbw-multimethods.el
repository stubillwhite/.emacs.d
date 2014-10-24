(require 'sbw-hash-tables)
(require 'dash)

(defun sbw/-mm-set-registry (r)
  (setq *sbw/-mm-registry* r))

(defun sbw/-mm-get-registry ()
  (defvar *sbw/-mm-registry* (sbw/ht-create))
  *sbw/-mm-registry*)






(defun sbw/-mm-register-dispatch-handler (f-symbol dispatch-val handler-f)
  (-> (sbw/-mm-get-registry)
    (sbw/ht-assoc-in (list f-symbol dispatch-val) handler-f)
    (sbw/-mm-set-registry)))

(defun sbw/-mm-register-multimethod (f)
  (-> (sbw/-mm-get-registry)
    (sbw/ht-assoc f (sbw/ht-create))
    (sbw/-mm-set-registry)))

(defun sbw/-mm-get-handler (f-symbol dispatch-func args)
  (let* ( (dispatch-val (apply dispatch-func args)) )
    (-> (sbw/-mm-get-registry)
      (sbw/ht-get-in (list f-symbol (vector dispatch-val))))))


(defmacro sbw/mm-defmethod (name dispatch-val signature handler-f)
  (let ( (f-symbol (intern (symbol-name name))) )
    `(sbw/-mm-register-dispatch-handler (quote ,f-symbol) ,dispatch-val (lambda ,signature ,handler-f))))


(defmacro sbw/-mm-create-multimethod-function (f-symbol dispatch-func)
  `(defun ,f-symbol (&rest args)
     (let* ( (handler-f (sbw/-mm-get-handler (quote ,f-symbol) ,dispatch-func args)) )
       (apply handler-f args))))

(defmacro sbw/mm-defmulti (name dispatch-func)
  (let ( (f-symbol (intern (symbol-name name))) )
    `(progn
       (sbw/-mm-register-multimethod (quote ,f-symbol))
       (sbw/-mm-create-multimethod-function ,f-symbol ,dispatch-func))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parity (x)
  (if (= 0 (mod x 2)) :even :odd))

;(setq *sbw/-mm-registry* (sbw/ht-create))

(sbw/mm-defmulti describe-number 'parity)
(sbw/mm-defmethod describe-number [:even] (x) (print (format "The number %d is even" x)))
(sbw/mm-defmethod describe-number [:odd]  (x) (print (format "The number %d is odd" x)))

(describe-number 1)
(describe-number 2)

(print *sbw/-mm-registry*)

(provide 'sbw-multimethods)
