;;;; http://nostdal.org/Aromyxo/ ;;;;

(in-package :am-util)

(defparameter *referencables*
  (make-hash-table :test #'equal))



(defclass referencable ()
  ((id :initform (generate-id) :reader id-of))
  (:documentation "Object is globally referencable and has an unique ID."))
(export '(referencable id-of))



(defmethod initialize-instance :after ((referencalbe referencable) &key)
  (setf (gethash (id-of referencalbe) *referencables*)
        referencalbe))



(defmethod object-with-id ((id integer))
  "Return object with `id'."
  (gethash id *referencables*))
(export 'object-with-id)



(defmethod remove-object-with-id ((id integer))
  "Object with `id' can no longer be reached by `getObj'."
  (remhash id *referencables*))
(export 'remove-object-with-id)



(defmethod remove-object ((obj referencable))
  "`obj' can no longer be reached by `object-with-id'."
  (remhash (id-of obj) *referencables*))
(export 'remove-object)

