;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed))


(defstruct (pointer (:constructor mk-pointer (&optional value))
                    (:conc-name :ptr-)
                    (:copier nil))
  (value))
(export '(pointer mk-pointer ptr-value))


(declaim (inline mk-ptr))
(defun mk-ptr (&optional value)
  (mk-pointer value))
(export 'mk-ptr)


(defmethod deref ((pointer pointer))
  (ptr-value pointer))
(export 'deref)


(defmethod (setf deref) (new-value (pointer pointer))
  (setf (ptr-value pointer) new-value))