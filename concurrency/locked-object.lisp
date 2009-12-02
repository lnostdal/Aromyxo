;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=
(declaim (optimize speed))


(define-variable -object-locks-
    :value (make-hash-table :test #'eq
                            :synchronized t
                            :weakness :key))


(defmacro with-locked-object (locked-object &body body)
  `(with-recursive-lock-held ((lock-of ,locked-object))
     ,@body))



(defclass locked-object ()
  ((lock :reader lock-of)))


(defmethod initialize-instance :before ((locked-object locked-object) &key)
  (setf (slot-value locked-object 'lock)
        (make-recursive-lock (princ-to-string locked-object))))


(defmethod lock-of (object)
  (multiple-value-bind (lock found-p) (gethash object -object-locks-)
    (if found-p
        (values lock :found)
        (sb-ext:with-locked-hash-table (-object-locks-)
          (multiple-value-bind (lock found-p) (gethash object -object-locks-)
            (if found-p
                (values lock :found)
                (values (setf (gethash object -object-locks-)
                              (make-recursive-lock (princ-to-string object)))
                        :created)))))))
