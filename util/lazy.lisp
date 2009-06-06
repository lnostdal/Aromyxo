;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed (space 0) (safety 0)))


(defstruct (lazy-value (:constructor %mk-lazy-value (level value-fn))
                       (:conc-name :lv-))
  (value-fn (lambda () (values)) :type function)
  (cached-value nil :type t)
  (level 1 :type integer))


(defmacro mk-lazy-value (lazy-level &body value-form)
  `(%mk-lazy-value ,lazy-level
                   (lambda () ,@value-form)))


(declaim (inline get-lazy-value))
(defun get-lazy-value (lazy-value)
  (if (plusp (lv-level lazy-value))
      (progn
        (decf (lv-level lazy-value))
        (setf (lv-cached-value lazy-value)
              (funcall (the function (lv-value-fn lazy-value)))))
      (lv-cached-value lazy-value)))


(defmethod deref-expand ((arg symbol) (type (eql 'lazy-value)))
  `(get-lazy-value ,arg))


(defmethod deref ((lazy-value lazy-value))
  (get-lazy-value lazy-value))
