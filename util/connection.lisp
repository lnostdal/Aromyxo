;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(mk-meta-slot outgoing-connections-of)
(mk-meta-slot incoming-connections-of)


(let ((lock (make-lock "AM-UTIL:MK-CONNECTION")))
  (defun mk-connection (from to &key (test #'eq))
    "After calling this, (OUTGOING-CONNECTIONS-OF FROM) will contain a
\"connection\" from FROM to TO, and (INCOMING-CONNECTIONS-OF TO) will contain
a \"connection\" from TO to FROM.

If either FROM or TO is GCed the connections between them is also GCed or
removed."
    (declare (optimize speed))
    (with-lock-held (lock)
      ;; FROM --> TO
      (multiple-value-bind (outgoing-connections found-p) (outgoing-connections-of from)
        (if found-p
            (setf (gethash to outgoing-connections) from)
            (let ((outgoing-connections (make-hash-table :test test :weakness :key-and-value
                                                         :synchronized t)))
              (setf (outgoing-connections-of from) outgoing-connections
                    (gethash to outgoing-connections) from))))

      ;; FROM <-- TO
      (multiple-value-bind (incoming-connections found-p) (incoming-connections-of to)
        (if found-p
            (setf (gethash from incoming-connections) to)
            (let ((incoming-connections (make-hash-table :test test :weakness :key-and-value
                                                         :synchronized t)))
              (setf (incoming-connections-of to) incoming-connections
                    (gethash from incoming-connections) to))))
      (values))))



(defun rm-connection (from to)
  (declare (optimize speed))
  (when-let (outgoing-connections (outgoing-connections-of from))
    (remhash from outgoing-connections))
  (when-let (incoming-connections (incoming-connections-of to))
    (remhash to incoming-connections)))


(defun rm-connections (object)
  "Remove any connections going to OBJECT and any connections going from OBJECT."
  (declare (optimize speed))
  ;; OBJECT --/-->> "them"
  (when-let (outgoing-connections (outgoing-connections-of object))
    (maphash (lambda (target not-used)
               (declare (ignore not-used))
               (remhash object (incoming-connections-of target)))
             outgoing-connections))

  ;; OBJECT <<--/-- "them"
  (when-let (incoming-connections (incoming-connections-of object))
    (maphash (lambda (source not-used)
               (declare (ignore not-used))
               (remhash object (outgoing-connections-of source)))
             incoming-connections)))


(defmacro with-targets-of ((from &key (target-sym 'target)) &body body)
  (with-gensyms (source-sym)
    `(maphash (lambda (,target-sym ,source-sym)
                (declare (ignore ,source-sym)) ;; We already know; it's the same value as FROM.
                ,@body)
              (outgoing-connections-of ,from))))


(defmacro with-sources-of ((to &key (source-sym 'source)) &body body)
  (with-gensyms (target-sym)
    `(maphash (lambda (,source-sym ,target-sym)
                (declare (ignore ,target-sym)) ;; We already know; it's the same value as TO.
                ,@body)
              (incoming-connections-of ,to))))
