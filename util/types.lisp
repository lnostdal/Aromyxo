;;;; http://nostdal.org/ ;;;;

(in-package :am-util)


(defclass AssertSlot (standard-class) 
  ()
  (:documentation "Usage:
\(defclass Header ()
\  ((name :type string :accessor name :initform nil :initarg :name) 
\   (value :accessor value :initform nil :initarg :value))
\  (:metaclass AssertSlot))"))
(export 'AssertSlot)


(defmethod closer-mop:validate-superclass ((sub AssertSlot) (sup standard-class))
  t)

(define-condition SlotTypeError (type-error)
  ()
  (:report (lambda (condition stream)
             (format stream "A value of ~A doesnt match slot type ~A"
                     (type-error-datum  condition)
                     (type-error-expected-type condition)))))

(defmethod (setf closer-mop:slot-value-using-class) :after
    (new-value (class AssertSlot) instance slot)
  (declare (ignore instance))
  (let ((slot-type (closer-mop:slot-definition-type slot)))
    (assert (typep new-value slot-type) (new-value)
            'SlotTypeError :datum new-value :expected-type slot-type)))