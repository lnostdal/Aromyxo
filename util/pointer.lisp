;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed))


(defstruct (pointer (:constructor mk-pointer (&optional value))
                    (:conc-name :ptr-)
                    (:copier nil))
  (value))
(export '(pointer mk-pointer ptr-value))


(declaim (inline mk-ptr))
(defun mk-ptr (&optional value)
  (mk-pointer value))
(export 'mk-ptr)


(defmethod deref-expand ((arg symbol) (type (eql 'pointer)))
  `(ptr-value ,arg))


(defmethod deref ((pointer pointer))
  (ptr-value pointer))
(export 'deref)


(defmethod (setf deref) (new-value (pointer pointer))
  (setf (ptr-value pointer) new-value))


;; TODO: Maybe this doesn't make much sense; I got to take a look at DEFSETF and
;; DEFINE-SETF-EXPANDER.
(defmacro place-fn (place-form)
  "This creates a closure which can write to and read from the \"place\"
designated by PLACE-FORM."
  (with-gensyms (value value-supplied-p)
    `(lambda (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
           ,place-form))))
(export 'place-fn)
