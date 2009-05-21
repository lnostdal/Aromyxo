;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(declaim (optimize speed))


(defvar *object->waitqueue*
  (make-hash-table :test #'eq
                   ;; Mutual exclusion of concurrent SB-EXT:WITH-LOCKED-HASH-TABLE bodies.
                   :synchronized nil
                   :weakness :key))
(export '*object->waitqueue*)


(defun waitqueue-of (object &key name)
  "OBJECT is the thing which holds or \"represents\" the queue.
This will return a unique designated waitqueue for OBJECT."
  (multiple-value-bind (condition found-p) (gethash object *object->waitqueue*)
    (if found-p
        (values condition
                :found)
        (sb-ext:with-locked-hash-table (*object->waitqueue*)
          (multiple-value-bind (condition found-p) (gethash object *object->waitqueue*)
            (if found-p
                (values condition
                        :found)
                (values (setf (gethash object *object->waitqueue*)
                              (make-waitqueue :name name))
                        :created)))))))
(export 'waitqueue-of)


(defun condition-wait (object lock)
  "OBJECT is the thing which holds or \"represents\" the queue.
LOCK must be held when calling this.
NOTE: You might want to use the WITH-WAITQUEUE macro instead of this.
Note that this will not check if CONDITION-NOTIFY has already been called for
OBJECT."
  (declare (mutex lock))
  (sb-thread:condition-wait (waitqueue-of object)
                            lock))
(export 'condition-wait)


(defun condition-notify (object &key (number 1))
  (declare (fixnum number))
  "OBJECT is the thing which holds or \"represents\" the queue.
The lock held when CONDITION-WAIT was called must be held when this is
called. NUMBER refers to the number of threads waiting on OBJECT we should
notify or attempt to wake up."
  (sb-thread:condition-notify (waitqueue-of object)
                              number))
(export 'condition-notify)


(defun condition-broadcast (object)
  "OBJECT is the thing which holds or \"represents\" the queue.
The lock held when calling CONDITION-WAIT must be held when this is called."
  (sb-thread:condition-broadcast (waitqueue-of object)))
(export 'condition-broadcast)


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
(export 'with-waitqueue)
