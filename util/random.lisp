;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun random-between (min max)
  (declare (number min max))
  (the number (+ (random (- max min)) min)))
