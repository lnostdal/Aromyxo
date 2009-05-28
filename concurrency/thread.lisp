;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-symbol-macro *current-thread*
    #+sbcl sb-thread:*current-thread*)
(export '*current-thread*)


;; TODO: Bordeaux-Threads suddenly creates a new (non-random ....) *random-state*
;; on thread creation for no good reason now. I can't deal with this B.S. right now
;; so we'll just call sb-thread:make-thread directly instead .....
(defmacro with-thread ((name &rest forward-dynamic-bindings) &body body)
  "Defines a thread that executes `body'. Returns the thread-instance."
  (if (null forward-dynamic-bindings)
      `(sb-thread:make-thread (lambda () ,@body) :name ,name)
      (let ((temp-names (loop :repeat (length forward-dynamic-bindings) :collect (gensym))))
        `(let (,@(mapcar (lambda (x y)
                           (list x y))
                         temp-names
                         forward-dynamic-bindings))
           (sb-thread:make-thread (lambda ()
                          (let (,@(mapcar (lambda (x y)
                                            (list x y))
                                          forward-dynamic-bindings
                                          temp-names))
                            ,@body))
                        :name ,name)))))
(export 'with-thread)


(defmacro with-sthread (&body body)
  "Define and start a thread as simple as possible."
  `(sb-thread:make-thread (lambda () ,@body)))
(export 'with-sthread)









#|
(defmacro interrupt-thread (thread &body body)
  `(sb-thread:interrupt-thread ,thread (lambda () ,@body)))
(export 'interrupt-thread)


(defmacro join-threads (&body threads)
  `(progn
     ,@(mapcar (lambda (thread) `(join-thread ,thread))
              threads)))
(export 'join-threads)


(defvar *threads* (make-hash-table))
(export '*threads*)


(defclass thread ()
  ((thread :reader thread-of)
   (groups :accessor groups-of)))
(export '(thread thread-of id-of groups-of))


(defmethod main ((thread thread))
  (error "Overide method `main' for `~A'~%" thread))
(export 'main)


(defmethod initialize-instance :after ((thread thread) &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (setf (slot-value thread 'thread)
        (with-thread ((slot-value thread 'id))
          (setf (gethash *current-thread* *threads*) thread)
          (unwind-protect
               (main thread)
            (remhash *current-thread* *threads*)))))


(defmethod instance-of ((low-level-thread sb-thread:thread))
  "Returns the instance associated with a low-level thread."
  (gethash low-level-thread *threads*))
(export 'instance-of)

|#