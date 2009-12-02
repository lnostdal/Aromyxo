;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


;; NOTE: There is a chance this stuff isn't needed; i.e., slot-access in SBCL
;; might be thread-safe already.

(defclass mutex-slot-mixin-class ()
  ())


(defmethod slot-value-using-class :around ((class mutex-slot-mixin-class) instance slot-definition)
  (with-recursive-lock-held ((lock-of instance))
    (call-next-method)))


(defmethod (setf slot-value-using-class) :around (new-value (class mutex-slot-mixin-class) instance slot-definition)
  (with-recursive-lock-held ((lock-of instance))
    (call-next-method)))
