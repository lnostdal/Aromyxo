;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro define-variable (name &key
                           (value nil value-supplied-p)
                           (kind :var)
                           (doc nil doc-supplied-p)
                           (type nil type-supplied-p)
                           (always-boundp value-supplied-p)
                           (test nil test-supplied-p))
  ;; TODO: Slimes indentation is horrible here.
  `(progn
     ,(when type-supplied-p
            `(eval-now (proclaim '(,type ,name))))

     ,(case kind
            (:var
             `(defvar ,name
                ,@(when value-supplied-p `(,value))
                ,@(when doc-supplied-p `(,doc))))

            (:parameter
             (assert value-supplied-p)
             `(defparameter ,name ,value
                ,@(when doc-supplied-p `(,doc))))

            (:constant
             (assert value-supplied-p)
             (nilf always-boundp)
             `(define-constant ,name ,value
                ,@(when test-supplied-p `(:test ,test))
                ,@(when doc-supplied-p `(:documentation ,doc))))

            (:global
             `(sb-ext:defglobal ,name ,value
                ,@(when doc-supplied-p `(,doc)))))

     ,@(when always-boundp
             (assert value-supplied-p)
             `((proclaim '(sb-ext:always-bound ,name))))))
(export 'define-variable)


(defmacro define-global (var value &optional doc)
  (warn "DEFINE-GLOBAL is deprecated; use DEFINE-VARIABLE
with :GLOBAL supplied for the :KIND arg.")
  `(sb-ext:defglobal ,var ,value ,doc))
(export 'define-global)
