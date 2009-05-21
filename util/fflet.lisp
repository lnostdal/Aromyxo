;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro fflet (bindings &body body)
  (let ((tmp-funcs (mapcar (lambda (binding) (gensym (symbol-name (first binding)))) bindings)))
    `(let (,@(mapcar (lambda (tmp-func binding)
                       `(,tmp-func ,(second binding)))
                     tmp-funcs bindings))
       (macrolet (,@(mapcar (lambda (binding tmp-func)
                              `(,(first binding) (&rest args)
                                 `(funcall ,',tmp-func ,@args)))
                            bindings tmp-funcs))
         ,@body))))
(export 'fflet)


#|
(defun make-adder (n)
  "Returns a function that takes one argument and returns the sum of that argument and `n'."
  (lambda (x) (+ x n)))


(defun test ()
  (fflet ((add-2 (make-adder 2))
          (add-3 (make-adder 3)))
    (list (add-2 2)
          (add-3 2))))


(test) => (4 5)
|#
