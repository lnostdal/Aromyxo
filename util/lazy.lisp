;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed (space 0) (safety 0)))


(defstruct (lazy-value (:constructor %mk-lazy-value (value-fn))
                       (:conc-name :%lazy-value-))
  (value-fn (lambda () (values)) :type function))


(defmacro mk-lazy-value (&body value-form)
  `(%mk-lazy-value (lambda () ,@value-form)))


(declaim (inline get-lazy-value))
(defun get-lazy-value (lazy-value)
  (funcall (the function (%lazy-value-value-fn lazy-value))))


(defmacro set-lazy-value (lazy-value &body value-form)
  `(setf (%lazy-value-value-fn ,lazy-value)
         (lambda () ,@value-form)))


(defmethod deref-expand ((arg symbol) (type (eql 'lazy-value)))
  `(get-lazy-value ,arg))


(defmethod deref ((lazy-value lazy-value))
  (get-lazy-value lazy-value))
