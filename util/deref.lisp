;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

#|
(defparameter *extract-model-p* nil)
(export '*extract-model-p*)
|#


(defmacro with-object (object &body body)
  `(let ((%with-object ,object))
     ,@body))
(export 'with-object)


(defun full-deref (object)
  (let ((value (deref object)))
    (loop :while (find-method #'deref nil (list (class-of value)) nil)
       :do (setf value (deref value)))
    value))

#|
(defmacro with-model-of (&body body)
  `(let ((*extract-model-p* t))
     ,@body))
|#