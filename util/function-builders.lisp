;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(defmacro iambda (&body body)
  "A function that ignores any arguments given to it. An \"ignoring lambda\"."
  (let ((rest (gensym)))
    `(lambda (&rest ,rest)
       (declare (ignore ,rest))
       ,@body)))



(defmacro as-function (&body body)
  "The value of `body' is returned as a function that reveals the value once called."
  `(lambda () ,@body))


(defun always (x)
  "Returns a function that always returns 'x'. (identity)"
  (lambda (&rest args)
    (declare (ignore args))
    x))


(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
      (,op ,@(mapcar (lambda (f)
                       `(,(rbuild f) ,g))
                     fns)))))


(defun rbuild (expr)
  (if (or (atom expr)
          (eq (car expr) 'lambda))
      expr
      (build-call (car expr) (cdr expr))))


(defmacro fn (expr)
  `#',(rbuild expr))


(defmacro rlambda (args &body body)
  "Note that Alexandria has a NAMED-LAMBDA macro."
  `(labels ((recur ,args
              ,@body))
     #'recur))
