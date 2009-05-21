;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro iambda (&body body)
  "A function that ignores any arguments given to it. An \"ignoring lambda\"."
  (let ((rest (gensym)))
    `(lambda (&rest ,rest)
       (declare (ignore ,rest))
       ,@body)))
(export 'iambda)



(defmacro as-function (&body body)
  "The value of `body' is returned as a function that reveals the value once called."
  `(lambda () ,@body))
(export 'as-function)


(defun always (x)
  "Returns a function that always returns 'x'. (identity)"
  (lambda (&rest args)
    (declare (ignore args))
    x))
(export 'always)


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
(export 'fn)


(defmacro rlambda (args &body body)
  `(labels ((recur ,args
              ,@body))
     #'recur))
(export '(rlambda recur))
