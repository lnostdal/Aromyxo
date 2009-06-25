;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed (safety 1)))


(defstruct (atomt (:constructor mk-atom (&optional value))
                  (:copier nil))
  (value))
(export '(atomt mk-atom atomt-p atomt-value))


(defmethod deref-expand ((arg symbol) (type (eql 'atomt)))
  `(atomt-value ,arg))


(defmethod deref ((atomt atomt))
  (atomt-value atomt))
(export 'deref)


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
  "NEW must be a value that yields T for (EQ NEW NEW).
Note that NEW might be evaluated more than once."
  (once-only (atom)
    `(swap (atomt-value ,atom) ,new)))
(export 'swap-atom)


(declaim (inline swap-atom-fn))
(defun swap-atom-fn (atom fn)
  "FN is a function that accepts one argument; the old value of ATOM. It must
return a value that yields T for (EQ VALUE VALUE). Note that it might be called
more than once and the old value passed to the function might change for each
call."
  (declare (atomt atom)
           (function fn))
  (swap-atom atom (funcall fn %old)))
(export 'swap-atom-fn)
