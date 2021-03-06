;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun smallest (a &rest args)
  (let ((till-now a))
    (dolist (item args)
      (if (< item till-now)
          (setf till-now item)))
    till-now))


(defun biggest (a &rest args)
  (let ((till-now a))
    (dolist (item args)
      (if (> item till-now)
          (setf till-now item)))
    till-now))


;; NOTE: For every float dividable by 256 this'll return a float with a 0 fraction.
(defun urandom (limit)
  (let ((result
         (with-open-file (fs "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
           (let* ((num (/ limit 256))
                  (ceil (ceiling num)))
             (* (loop :repeat ceil :summing (read-byte fs))
                (/ limit (* ceil 256)))))))
    (if (integerp limit)
        (values (truncate result))
        result)))


(declaim (inline range))
(defun range (from &optional to (by 1))
  (loop :for i :from (if to from 0) :to (if to to from) :by by
     :collect i))


(declaim (inline product))
(defun product (l)
  (apply #'* l))


(declaim (inline sum))
(defun sum (l)
  (apply #'+ l))
