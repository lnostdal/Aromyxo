;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)
(declaim (optimize speed))


(defstruct (pointer (:constructor mk-ptr (&optional value))
                    (:conc-name :ptr-)
                    (:copier nil))
  (value))
(export '(pointer mk-ptr ptr-value))


(defmethod deref-expand ((arg symbol) (type (eql 'pointer)))
  `(ptr-value ,arg))


(defmethod deref ((pointer pointer))
  (ptr-value pointer))
(export 'deref)


(defmethod (setf deref) (new-value (pointer pointer))
  (setf (ptr-value pointer) new-value))


(defmacro place-fn (place-form)
  "This creates a closure which can write to and read from the \"place\"
designated by PLACE-FORM."
  (with-gensyms (value value-supplied-p)
    `(lambda (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
           ,place-form))))
(export 'place-fn)
