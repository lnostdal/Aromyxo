;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defun random-between (min max)
  (declare (number min max))
  (the number (+ (random (- max min)) min)))
(export 'random-between)
