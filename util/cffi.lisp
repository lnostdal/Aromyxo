;;;; http://nostdal.org/aromyxo/ ;;;;

(in-package :AmUtil)


(defvar *lambda-callbacks* nil
  "TODO: Clean up later somehow?") (export '*lambda-callbacks*)


;; Suggest for inclusion in cffi?
(defmacro cffi::lambda-callback (return-type args &body body)
  (let ((name (gensym)))
    `(progn
       (cffi:defcallback ,name ,return-type ,args
         ,@body)
       (push ',name *lambda-callbacks*)
       (cffi:callback ,name))))
(export 'cffi::lambda-callback 'cffi)
