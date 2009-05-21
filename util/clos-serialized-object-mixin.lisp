;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-unbound-var *serialized-mixin-class-data*)


(defmacro with-serialized-classes (&body body)
  `(let ((*serialized-mixin-class-data* (mk-pointer nil)))
     (unwind-protect (progn ,@body)
       (dolist (instance (ptr-value *serialized-mixin-class-data*))
         (with-locked-object instance
           (condition-notify instance))))))


(defclass serialized-mixin-class (standard-class)
  ())


(defmethod validate-superclass ((class serialized-mixin-class) (superclass standard-class))
  t)


(defmethod (setf slot-value-using-class) :around (new-value (class serialized-mixin-class) instance slot-definition)
  (if (with-locked-object *serialized-mixin-class-data*
        (sb-thread:holding-mutex-p (lock-of instance)))
      (call-next-method)
      (progn
        (with-locked-object *serialized-mixin-class-data*
          (push instance (ptr-value *serialized-mixin-class-data*)))
        (am-conc:acquire-lock (lock-of instance))
        (call-next-method))))
    


(defclass blah ()
  ((a :accessor a-of))

  (:metaclass serialized-mixin-class))



(defun test ()
  (with-serialized-classes 
      (let ((test-1 (make-instance 'blah)))
        (setf (a-of test-1) "aoeu")
        (setf (a-of test-1) "1234")
        (a-of test-1))))
