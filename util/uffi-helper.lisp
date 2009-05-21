;; http://nostdal.org/uffi-helper

;(in-package #:Util)


(let ((types 
       ;; UFFI-types that need some handling from the lisp-side.
       (list :double :single)))
  (defmacro def-function (names args &key module returning)
    ;; TODO: Check if any of the args have a type that needs conversion.
    (dolist (i args)
      (format t "~A~%" (second i)))
    `(progn
      (uffi:def-function ,(list (first names) 
                                (read-from-string (concatenate 'string "%" 
                                                               (string-downcase 
                                                                (symbol-name (second names)))))) ,args 
        ,@(if module (list :module module) (values))
        ,@(if returning (list :returning returning) (values)))
      (defun ,(second names) ,args))))
  

