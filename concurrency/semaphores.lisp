;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defvar *object-semaphores*
  (make-hash-table :test #'eq
                   ;; Mutual exclusion of concurrent SB-EXT:WITH-LOCKED-HASH-TABLE bodies.
                   :synchronized nil
                   :weakness :key))
(export '*object-semaphores*)


(defun semaphore-of (object &key (count 0) name)
  "This will return a unique designated semaphore for OBJECT.
If a new one is created, its initial count will be COUNT."
  (declare (optimize speed)
           (fixnum count))
  (multiple-value-bind (semaphore found-p) (gethash object *object-semaphores*)
    (if found-p
        (values semaphore
                :found)
        (sb-ext:with-locked-hash-table (*object-semaphores*)
          (multiple-value-bind (semaphore found-p) (gethash object *object-semaphores*)
            (if found-p
                (values semaphore
                        :found)
                (values (setf (gethash object *object-semaphores*)
                              (make-semaphore :name name :count count))
                        :created)))))))
(export 'semaphore-of)


(defun mk-semaphore (&key name (count 0))
  (make-semaphore :name name :count count))
(export 'mk-semaphore)


(defun semaphore-signal (semaphore &optional (n 1))
  (declare (optimize speed)
           (semaphore semaphore)
           (fixnum n))
  (sb-thread:signal-semaphore semaphore n))
(export 'semaphore-signal)


(defun semaphore-wait (semaphore)
  (declare (optimize speed)
           (semaphore semaphore))
  (sb-thread:wait-on-semaphore semaphore))
(export 'semaphore-wait)
