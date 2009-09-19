;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro with-object (object &body body)
  `(let ((%with-object ,object))
     (declare (ignorable %with-object))
     ,@body))
(export 'with-object)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod deref-expand ((arg symbol) type)
    nil))


;; TODO: Finish this.
(define-compiler-macro deref (&whole form arg &environment env)
  (if (atom arg)
      (let ((type (lex-var-info arg env)))
        (if-let (body (deref-expand arg type))
          body
          form))
      form))


(defmethod deref-expand ((arg symbol) (type (eql 'function)))
  `(funcall ,arg))


(defmethod deref ((fn function))
  (funcall fn))


(defmethod (setf deref) (new-value (fn function))
  (funcall fn new-value))


(defmethod deref ((obj t))
  obj)


(defmethod deref ((list list))
  (mapcar #'funcall list))


(defmethod (setf deref) (arg (list list))
  (mapcar (lambda (fn) (funcall fn arg))
          list))


(defun full-deref (object)
  (let ((value (deref object)))
    (loop :while (find-method #'deref nil (list (class-of value)) nil)
       :do (setf value (deref value)))
    value))


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
