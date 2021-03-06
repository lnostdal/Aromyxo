;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defmacro while (pred &body body)
  (let ((result (gensym)))
    `(let ((,result nil))
       (do () ((not ,pred) ,result)
         (setf ,result (progn ,@body))))))
