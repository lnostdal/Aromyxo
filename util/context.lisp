;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(eval-now
  (defclass context ()
    ((bindings :accessor bindings-of :initarg :bindings
               :initform nil))))


(define-variable *context*
    :value nil
    :type (or context null)
    :doc "The current CONTEXT, or NIL if there is no current CONTEXT.")


(defmethod context-start ((ctx context))
  )


(defmethod context-cleanup ((ctx context))
  )


(defmethod context-body ((ctx context) body)
  (let ((*context* ctx))
    (labels ((context-init (bindings)
               (funcall (car bindings) (lambda () (context-init (cdr bindings))))))
      (context-init (reverse (cons (lambda (cnt)
                                     (declare (ignore cnt)) ;; End of the line.
                                     (unwind-protect
                                          (progn
                                            (context-start ctx)
                                            (funcall body))
                                       (context-cleanup ctx)))
                                   (bindings-of ctx)))))))


(defmacro with-context (ctx &body body)
  `(context-body ,ctx (lambda () ,@body)))
