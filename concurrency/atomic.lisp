;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)
(declaim (optimize speed (safety 1)))


(defstruct (atomt (:constructor mk-atom (&optional value))
                  (:copier nil))
  (value))
(export '(atomt mk-atom atomt-p atomt-value))


(add-deref-type 'atomt
                :get-expansion (λ (arg-sym) `(atomt-value ,arg-sym))
                :set-expansion t)


(defmacro %swap (place new)
  "This binds %OLD. NEW might be evaluated more than once."
  (with-gensyms (mnew)
    `(loop
        (let* ((%old ,place)
               (,mnew ,new))
          (when (eq %old (sb-ext:compare-and-swap ,place %old ,mnew))
            (return ,mnew))))))


(defmacro swap-atom (atom new)
  "NEW must be a value that yields T for (EQ NEW NEW).
NEW might be evaluated more than once."
  (once-only (atom)
    `(%swap (atomt-value ,atom) ,new)))
(export 'swap-atom)


(declaim (inline swap-atom-fn))
(defun swap-atom-fn (atom fn)
    "FN is a function that accepts one argument; the old or current value of ATOM.
It must return a value that yields T for (EQ VALUE VALUE). Note that FN might be
called more than once and the argument passed to it might change for each call."
    (declare (atomt atom)
             (function fn))
    (swap-atom atom (funcall fn %old)))
(export 'swap-atom-fn)
