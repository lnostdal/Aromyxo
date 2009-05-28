;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro with-object (object &body body)
  `(let ((%with-object ,object))
     ,@body))
(export 'with-object)


(defun full-deref (object)
  (let ((value (deref object)))
    (loop :while (find-method #'deref nil (list (class-of value)) nil)
       :do (setf value (deref value)))
    value))


(defmethod deref-expand ((arg symbol) type)
  nil)


(define-compiler-macro deref (&whole form arg &environment env)
  (if (atom arg)
      (let ((type (cdr (assoc 'type (third (multiple-value-list (sb-cltl2:variable-information arg env)))))))
        (if-let (body (deref-expand arg type))
          body
          form))
      form))



;; TODO: Finish this and add expanders to the appropriate places.
#|
(define-compiler-macro (setf deref) (&whole form arg arg2 &environment env)
  (dbg-princ form)
  (dbg-princ arg)
  (dbg-princ arg2)
  (let ((type (cdr (assoc 'type (third (multiple-value-list (sb-cltl2:variable-information
                                                             arg2
                                                             env)))))))
    (dbg-princ type))
  form)
|#