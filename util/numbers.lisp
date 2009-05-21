;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defun smallest (a &rest args)
  (let ((till-now a))
    (dolist (item args)
      (if (< item till-now)
          (setf till-now item)))
    till-now))
(export 'smallest)


(defun biggest (a &rest args)
  (let ((till-now a))
    (dolist (item args)
      (if (> item till-now)
          (setf till-now item)))
    till-now))
(export 'biggest)


