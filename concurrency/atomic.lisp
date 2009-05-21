;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed))


(defstruct (atomt (:constructor mk-atom (&optional value)))
  (value))
(export '(atomt mk-atom atomt-p atomt-value))


(defmethod deref ((atomt atomt))
  (atomt-value atomt))


(defmacro swap (place new)
  "NOTE: This binds %OLD, so this is not a macro intended for direct use by
outsiders.
Note that NEW might be evaluated more than once."
  (with-gensyms (mnew)
    `(loop
        (let* ((%old ,place)
               (,mnew ,new))
          (when (eq %old (sb-ext:compare-and-swap ,place %old ,mnew))
            (return ,mnew))))))


(defmacro swap-atom (atom new)
  "NEW must be something that yields a new and unique, on an EQ basis, value.
Note that NEW might be evaluated more than once."
  (once-only (atom)
    `(swap (atomt-value ,atom) ,new)))
(export 'swap-atom)


(defun swap-atom-fn (atom fn)
  "FN is a function that accepts one argument; the old value of ATOM. It must
return a new and unique, on an EQ basis, value. Note that it might be called
more than once."
  (declare (atomt atom)
           (function fn))
  (swap-atom atom (funcall fn %old)))
(export 'swap-atom-fn)
