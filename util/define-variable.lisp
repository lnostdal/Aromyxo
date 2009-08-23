;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro define-variable (name &key
                           (value nil value-supplied-p)
                           (kind :var)
                           (doc nil doc-supplied-p)
                           (type nil type-supplied-p)
                           always-boundp
                           (test nil test-supplied-p))
  ;; TODO: Slimes indentation is horrible here.
  `(progn

     ,(when type-supplied-p
            `(eval-now (proclaim '(,type ,name))))

     ,(case kind
            (:var
             `(eval-now
                (defvar ,name
                  ,@(when (or always-boundp value-supplied-p)
                          `(,value)))
                ,(when doc-supplied-p
                       `(setf (documentation ',name 'variable) ,doc))))

            (:parameter
             `(eval-now
                (defparameter ,name ,value
                  ,@(when doc-supplied-p `(,doc)))))

            (:constant
             `(define-constant ,name ,value
                ,@(when test-supplied-p `(:test ,test))
                ,@(when doc-supplied-p `(:documentation ,doc))))

            (:global
             `(sb-ext:defglobal ,name ,value
                ,@(when doc-supplied-p `(,doc)))))

     ,@(when (and always-boundp (eq kind (or :var :parameter)))
             `((eval-now (proclaim '(sb-ext:always-bound ,name)))))))
(export 'define-variable)


(defmacro define-global (var value &optional doc)
  (warn "DEFINE-GLOBAL is deprecated; use DEFINE-VARIABLE
with :GLOBAL supplied for the :KIND arg.")
  `(sb-ext:defglobal ,var ,value ,doc))
(export 'define-global)
