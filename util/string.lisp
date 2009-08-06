;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defparameter *newline-str*
  (format nil "~C" #\Newline))
(export '*newline-str*)


(defun fmtn (control-string &rest format-arguments)
  (apply #'format nil control-string format-arguments))
(export 'fmtn)


;; §4.7 Symbols and Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mkstr (&rest args) ;; TODO: Rename to rstr to contrast with cstr below?
  "Run-time string generation."
  (declare (optimize speed (safety 0))
           (dynamic-extent args))
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))
(export 'mkstr)


(defmacro cstr (&rest args)
  "Compile-time string generation."
  (with-output-to-string (str)
    (dolist (elt args)
      (princ elt str))))
(export 'cstr)


(defmacro catstr (&body args)
  ;; aks44; i stealed it .. lol
  `(concatenate 'string ,@args))
(export 'catstr)


(defmethod string<- (obj)
  (let ((*print-pretty* nil))
    (princ-to-string obj)))
(export 'string<-)