;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed))


(defparameter *object-locks*
  (make-hash-table :test #'eq
                   ;; Mutual exclusion of concurrent SB-EXT:WITH-LOCKED-HASH-TABLE bodies.
                   :synchronized nil
                   :weakness :key))
(export '*object-locks*)


(defmacro with-locked-object (locked-object &body body)
  `(with-recursive-lock-held ((lock-of ,locked-object))
     ,@body))
(export 'with-locked-object)


#|(defmacro with-locked-object (locked-object &body body)
  `(with-lock-held ((lock-of ,locked-object))
     ,@body))|#
#|(export 'with-locked-object)|#


(defclass locked-object ()
  ((lock :reader lock-of)))
(export '(locked-object lock-of))


(defmethod initialize-instance :before ((locked-object locked-object) &key)
  (setf (slot-value locked-object 'lock)
        (make-recursive-lock (princ-to-string locked-object))))


(defmethod lock-of (object)
  (declare (optimize speed))
  (multiple-value-bind (lock found-p) (gethash object *object-locks*)
    (if found-p
        (values lock :found)
        (sb-ext:with-locked-hash-table (*object-locks*)
          (multiple-value-bind (lock found-p) (gethash object *object-locks*)
            (if found-p
                (values lock :found)
                (values (setf (gethash object *object-locks*)
                              (make-lock (princ-to-string object)))
                        :created)))))))
(export 'lock-of)
