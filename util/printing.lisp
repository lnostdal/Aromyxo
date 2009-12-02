;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(defgeneric print-slots (object stream)
  (:method-combination progn))


(defmethod print-slots progn (object stream)
  )
