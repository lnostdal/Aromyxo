;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(let ((fun-lock (make-lock)))
  (defun acquire-recursive-lock (lock)
    (declare (optimize speed))
    (if (with-lock-held (fun-lock)
          (sb-thread:holding-mutex-p lock))
        nil
        (acquire-lock lock))))
(export 'acquire-recursive-lock)




#|
(defmacro make-mutex (&key (name nil name-supplied-p) (value nil value-supplied-p))
  (cond
    ((and name-supplied-p value-supplied-p)
     `(sb-thread:make-mutex :name ,name :value ,value))
    (name-supplied-p
     `(sb-thread:make-mutex :name ,name))
    (value-supplied-p
     `(sb-thread:make-mutex :value ,value))
    (t
     `(sb-thread:make-mutex))))
(export '(make-mutex mutex))



(defmethod name-of ((mutex sb-thread:mutex))
  (sb-thread:mutex-name mutex))
(export 'name-of)



(defmethod (setf name-of) (new-name (mutex sb-thread:mutex))
  (setf (sb-thread:mutex-name mutex) new-name))
(export 'name-of)



(defmethod value-of ((mutex sb-thread:mutex))
  (sb-thread:mutex-value mutex))
(export 'value-of)




(defmacro with-mutex ((mutex &key (new-value nil new-value-supplied-p) (wait-p t)) &body body)
  "Acquire `mutex' for the dynamic scope of `body', setting it to
`new-value' or some suitable default value if NIL.  If `wait-p' is non-NIL
and the mutex is in use, sleep until it is available"
  (if new-value-supplied-p
      `(sb-thread:with-mutex (,mutex :value ,new-value :wait-p ,wait-p)
         ,@body)
      `(sb-thread:with-mutex (,mutex :wait-p ,wait-p)
         ,@body)))
(export 'with-mutex)



(defmacro with-rmutex (mutex &body body)
  "Acquire `mutex' for the dynamic scope of `body'. It can be acquired multiple
times by the same thread."
  `(sb-thread:with-recursive-lock (,mutex)
     ,@body))
(export 'with-rmutex)

|#