;;;; http://nostdal.org/Aromyxo/ ;;;;

(in-package :aromyxo)
=common-headers=

;; These are modified "anaphoric" macros.


(defmacro aequal ((result-symbol &optional (test 'equal))
                  orig possibly-new
                  &body then-else)
  "Anaphoric equal-thing?"
  `(let ((,result-symbol ,possibly-new))
     (if (,test ,orig ,result-symbol)
         ,(first then-else)
         ,(second then-else))))


(defmacro aif (result-symbol test-form then &optional else)
  "Example:
\(aif answer (+ 2 2)
\     (format t \"answer is true, and has the value ~A~%\" answer)
\     (format t \"answer is false, or nil~%\"))"
  (declare (ignorable result-symbol))
  `(let ((,result-symbol ,test-form))
     (if ,result-symbol
         ,then
         ,else)))


(defmacro aif* (bindings &body then-else)
  `(let ,(mapcar (lambda (binding)
                   (first binding))
                 bindings)
     (if (and ,@(mapcar (lambda (binding)
                          `(setf ,(first binding) ,(second binding)))
                        bindings))
         ,(first then-else)
         ,(second then-else))))


(defmacro awhen (result-symbol test-form &body body)
  (warn "Aromyxo: Use WHEN-LET (from Alexandria) instead of AWHEN.")
  `(aif ,result-symbol ,test-form
     (progn ,@body)))


(defmacro awhen* (bindings &body body)
  `(aif* ,bindings
     ,@body))


(defmacro awhile (result-symbol test-form &body body)
  `(do ((,result-symbol ,test-form ,test-form))
    ((not ,result-symbol))
     ,@body))


(defmacro aand (result-symbol &rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,result-symbol
                 ,(car args)
                 (aand ,result-symbol ,@(cdr args))))))


(defmacro acond (result-symbol &body clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)) (sym (gensym)))
        `(let ((,sym ,(car cl1)))
          (if ,sym
              (let ((,result-symbol ,sym))
                ,@(cdr cl1))
              (acond ,result-symbol ,@(cdr clauses)))))))


(defmacro aif2 (result-symbol test-form
                &body then-else)
  "When `test-form' returns two values and either of them are T `then-form' is
evaluated - else `else-form' is evaluated."
  (let ((was-in (gensym)))
    `(mvbind (,result-symbol ,was-in) ,test-form
      (if (or ,was-in ,result-symbol)
          ,(first then-else)
          ,(second then-else)))))


(defmacro awhen2 (result-symbol test &body body)
  `(aif2 ,result-symbol ,test
    (progn ,@body)))


(defmacro aunless2 (result-symbol test &body body)
  `(aif2 ,result-symbol ,test
     nil
    (progn ,@body)))


(defmacro awhile2 (result-symbol test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
      (while ,flag
        (aif2 ,result-symbol ,test
              (progn ,@body)
              (setq ,flag nil))))))


(defmacro acond2 (result-symbol &body clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)) (found-p (gensym)))
        `(multiple-value-bind (,result-symbol ,found-p) ,(car cl1)
           (if (or ,result-symbol ,found-p)
               ,@(cdr cl1)
               (acond2 ,result-symbol ,@(cdr clauses)))))))


(defmacro self-ref (sym &body body)
  "Binds return-value of BODY to SYM."
  `(let (,sym) (setf ,sym ,@body)))


(defmacro alist (&rest items)
  "Anaphoric version of CL:LIST."
  (with-gensyms (oprev)
    `(let ((,oprev nil))
       (collecting
         ,@(loop :for item :in items
              :collect `(collect (let ((prev ,oprev))
                                   (declare (ignorable prev))
                                   (setf ,oprev ,item))))))))
