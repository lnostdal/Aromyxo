;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defun fmtn (control-string &rest format-arguments)
  (apply #'format nil control-string format-arguments))


;; §4.7 Symbols and Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mkstr (&rest args) ;; TODO: Rename to rstr to contrast with cstr below?
  "Run-time string generation."
  (declare (optimize speed (safety 0))
           (dynamic-extent args))
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))


(defmacro cstr (&rest args)
  "Compile-time string generation."
  (with-output-to-string (str)
    (dolist (elt args)
      (princ elt str))))


(defmacro catstr (&body args)
  `(concatenate 'string ,@args))


(defmethod string<- (obj)
  (if (stringp obj)
      obj
      (let ((*print-pretty* nil))
        (princ-to-string obj))))
