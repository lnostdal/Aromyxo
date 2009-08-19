;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

#|
TODO: Add support for type declarations for stuff like this:
        &OPTIONAL ((X FIXNUM) 42 X-SUPPLIED-P)
        &KEY ((Y FIXNUM) 42 Y-SUPPLIED-P)
|#


(defmacro defn (name args &body body)
  (let ((arg-types nil)
        (arg-names nil)
        (rtype (when (sequence-of-length-p args 2)
                 (prog1 (first args) (setf args (second args))))))

    ;; Parse argument list (ARGS).
    (dolist (arg args (setf arg-types (nreverse arg-types)
                            arg-names (nreverse arg-names)))
      (if (listp arg)
          (progn
            (push (first arg) arg-names)
            (push (second arg) arg-types))
          (progn
            (push arg arg-names)
            (push t arg-types))))

    ;; Check for old CL style declarations.
    (multiple-value-bind (body declarations doc)
        (parse-body body :documentation t :whole t)
      (dolist (declaration declarations)
        (warn "Found \"old\" CL style declaration in AMX:DEFN form: ~S~%It will be ignored.~%" declaration))
      (nilf declarations)

      ;; Generate type declarations for ARGS.
      (map nil (lambda (name type)
                 (push `(,type ,name) declarations))
           arg-names arg-types)

      ;; Generate type declaration for return value. TODO: Add support for multiple return values.
      (when rtype
        (push `(values ,rtype &optional) declarations))

      ;; Returned expansion/code.
      `(progn
         ;; Compile-time type checking. CL is wonderful! :)
         ,(when (or arg-types rtype)
           `(proclaim '(ftype (function (,@arg-types) ,@(when rtype `(,(first declarations))))
                        ,name)))
         (defun ,name ,arg-names
           ,@(when doc doc)
           ;; Run-time type checking, and/or generation of optimized/specialized code.
           ,@(when declarations `((declare ,@declarations)))
           ,@body)))))
(export 'defn)
