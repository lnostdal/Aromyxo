;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


;; TODO: Do the PROG1 thing here so the return values end up correct.

(defmacro define-variable (name &key
                           (value nil value-supplied-p)
                           (kind (case (char (string name) 0)
                                   (#\+ :constant)
                                   (#\- :global)
                                   (t :var)))
                           (doc nil doc-supplied-p)
                           (type nil type-supplied-p)
                           (always-boundp value-supplied-p)
                           (test nil test-supplied-p)
                           (retain-old-value-p t))
  ;; TODO: Slime indentation is horrible here.
  `(progn
     ,@(when type-supplied-p
             `((eval-now (proclaim '(,type ,name)))))
     (eval-now
       (prog1
           ,(case kind
                  (:var
                   `(eval-now
                      (prog1 (defvar ,name
                               ,@(when (or always-boundp value-supplied-p)
                                       `(,value))))
                      ,@(when doc-supplied-p
                              `((setf (documentation ',name 'variable) ,doc)))))

                  (:parameter
                   `(eval-now
                      (defparameter ,name ,value
                        ,@(when doc-supplied-p `(,doc)))))

                  (:constant
                   `(handler-bind ((simple-error (lambda (c)
                                                   (declare (ignore c))
                                                   ;; TODO: But, uh, how will we know this is the right condition?
                                                   ;; Alexandria doesn't use custom conditions for this ...
                                                   (when (and ,retain-old-value-p (find-restart 'ignore))
                                                     (invoke-restart (find-restart 'ignore))))))
                      (define-constant ,name ,value
                        ,@(when test-supplied-p `(:test ,test))
                        ,@(when doc-supplied-p `(:documentation ,doc)))))

                  (:global
                   `(sb-ext:defglobal ,name ,value
                      ,@(when doc-supplied-p `(,doc)))))

         ,@(when (and always-boundp (eq kind (or :var :parameter)))
                 `((eval-now (proclaim '(sb-ext:always-bound ,name)))))))))


(defmacro define-global (var value &optional doc)
  (warn "DEFINE-GLOBAL is deprecated; use DEFINE-VARIABLE
with :GLOBAL supplied for the :KIND arg.")
  `(sb-ext:defglobal ,var ,value ,doc))
