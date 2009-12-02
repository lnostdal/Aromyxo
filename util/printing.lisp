;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defgeneric print-slots (object stream)
  (:method-combination progn))


(defmethod print-slots progn (object stream)
  )
