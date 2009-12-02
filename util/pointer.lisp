;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=
(declaim (optimize speed))


(defstruct (pointer (:constructor mk-ptr (&optional value))
                    (:conc-name :ptr-)
                    (:copier nil))
  (value))


(add-deref-type 'pointer
                :get-expansion (λ (arg-sym) `(ptr-value ,arg-sym))
                :set-expansion t)


(defmacro place-fn (place-form)
  "This creates a closure which can write to and read from the \"place\"
designated by PLACE-FORM."
  (with-gensyms (value value-supplied-p)
    `(lambda (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
           ,place-form))))
