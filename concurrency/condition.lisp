;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=
(declaim (optimize speed))

;; We unintern here so that we don't re-define stuff from bordeaux-threads.


(define-variable -object->waitqueue-
    :value (make-hash-table :test #'eq
                            :synchronized t
                            :weakness :key))


(defun waitqueue-of (object &key name)
  "OBJECT is the thing which holds or \"represents\" the queue.
This will return a unique designated waitqueue for OBJECT."
  (multiple-value-bind (condition found-p) (gethash object -object->waitqueue-)
    (if found-p
        (values condition
                :found)
        (sb-ext:with-locked-hash-table (-object->waitqueue-)
          (multiple-value-bind (condition found-p) (gethash object -object->waitqueue-)
            (if found-p
                (values condition
                        :found)
                (values (setf (gethash object -object->waitqueue-)
                              (make-waitqueue :name name))
                        :created)))))))


(eval-now (unintern 'condition-wait))
(defun condition-wait (object lock)
  "OBJECT is the thing which holds or \"represents\" the queue.
LOCK must be held when calling this.
NOTE: You might want to use the WITH-WAITQUEUE macro instead of this.
Note that this will not check if CONDITION-NOTIFY has already been called for
OBJECT."
  (declare (mutex lock))
  (sb-thread:condition-wait (waitqueue-of object)
                            lock))


(eval-now (unintern 'condition-notify))
(defun condition-notify (object &key (number 1))
  (declare (fixnum number))
  "OBJECT is the thing which holds or \"represents\" the queue.
The lock held when CONDITION-WAIT was called must be held when this is
called. NUMBER refers to the number of threads waiting on OBJECT we should
notify or attempt to wake up."
  (sb-thread:condition-notify (waitqueue-of object)
                              number))


(eval-now (unintern 'condition-broadcast)) ;; This one is not from BT, but from SB-THREAD so it has a lock.
(defun condition-broadcast (object)
  "OBJECT is the thing which holds or \"represents\" the queue.
The lock held when calling CONDITION-WAIT must be held when this is called."
  (sb-thread:condition-broadcast (waitqueue-of object)))


(defmacro with-waitqueue ((object more-elements-check &key (lock `(lock-of ,object)))
                          &body body)
  "OBJECT is the thing which holds or \"represents\" the queue.
BODY should extract elements from the queue (and lock the queue itself when doing it;
but only temporarly for each iteration or extraction), until the queue is
determined to be empty.
MORE-ELEMENTS-CHECK should be given code which locks the queue and returns
T if there are more elements in the queue."
  `(loop
      ,@body
      (with-recursive-lock-held (,lock)
        (unless ,more-elements-check
          (sb-thread:condition-wait (waitqueue-of ,object) ,lock)))))



#|
;;; By using SB-QUEUE we don't have to lock as much:

(defclass worker (locked-object)
  ((queue :reader queue-of
          :initform (sb-queue:make-queue))))

(define-variable -worker- :value (make-instance 'worker))


(with-sthread
  (with-waitqueue (-worker-
                   (not (sb-queue:queue-empty-p (queue-of -worker-))))
    (block done
      (multiple-value-bind (value found-p)
          (sb-queue:dequeue (queue-of -worker-))
        (if found-p
            (dbg-prin1 value)
            (return-from done))))))


(sb-queue:enqueue 42 (queue-of -worker-))
(with-locked-object -worker- (condition-notify -worker-))
|#