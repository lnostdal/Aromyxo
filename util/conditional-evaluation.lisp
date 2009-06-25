;;;; http://nostdal.org/ ;;;

(in-package #:aromyxo)


(defmacro nth-expr (n &body expressions)
  `(case ,n
     ,@(let ((i -1))
         (mapcar (lambda (expr) `((,@(incf i)) ,expr))
                 expressions))
     (otherwise (error "`nth-expr': out of bounds."))))
(export 'nth-expr)


(defmacro nil-if (test-value value &optional (test 'equal))
  "If calling `test' with `value' and `test-value' as arguments returns T this
returns NIL, otherwise `value' is returned. The form `value' is only evaluated
once."
  (with-gensyms (res-value)
    `(let ((,res-value ,value))
       (if (,test ,res-value ,test-value)
           nil
           ,res-value))))
(export 'nil-if)


(defmacro nil-if-equal (test-value value &optional (test 'equal))
  "If calling `test' with `value' and `test-value' as arguments returns T this
returns NIL, otherwise `value' is returned. The form `value' is only evaluated
once."
  (with-gensyms (res-value)
    `(let ((,res-value ,value))
       (if (,test ,res-value ,test-value)
           nil
           ,res-value))))
(export 'nil-if-equal)


(defmacro value-if (test value-and-then &optional (else nil))
  "If calling `test' with `value-and-then' as argument returns T
return `value-and-then' else return `else'."
  (with-gensyms (res)
    `(let ((,res ,value-and-then))
       (if (,test ,res)
           ,res
           ,else))))
(export 'value-if)


(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))
(export 'if3)



(defmacro nif (expr pos zero neg)
  "\Numeric if\". Evaluates `pos', `zero' or `neg' depending on what `expr'
evaluates to."
  (with-gensyms (val)
    `(let ((,val ,expr))
      (cond ((plusp ,val) ,pos)
            ((zerop ,val) ,zero)
            (t ,neg)))))
(export 'nif)



(defmacro in ((obj &key (test 'equal) key) &rest choices)
  "Does calling `:test' with `obj' and any of `choices' as arguments return T?
:key defines how to access `choices'."
  (with-gensyms (insym)
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c)
                       `(,test ,insym ,(if key `(,key ,c) c)))
                     choices)))))
(export 'in)



(defmacro inq ((obj &key (test 'equal) (key 'self)) &rest choices)
  "Does the same as the macro `in', but choices are quoted."
  `(in (,obj :test ,test :key ,key) ,@(mapcar (lambda (a)
                                     `',a)
                                   choices)))
(export 'inq)



(defmacro in-if (fn &rest choices)
  (with-gensyms (fnsym)
    `(let ((,fnsym ,fn))
       (or ,@(mapcar (lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))
(export 'in-if)



(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq (key) t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))



(defmacro >case (expr &rest clauses)
  "A case where the keys are evaluated."
  (let ((g (gensym)))
    `(let ((,g ,expr))
      (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                      clauses)))))
(export '>case)


(defmacro any (&rest forms)
  "A version of CL:OR that doesn't short-circuit evaluation."
  (with-gensyms (elt)
    `(dolist (,elt (list ,@forms))
       (when ,elt (return ,elt)))))
(export 'any)
