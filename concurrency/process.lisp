;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


#| TODO:

  Each process should not directly contain or hold one thread, which might sit
around and idle for long periods of time anyway.

A thread is something which the process can request a need for and it may only
hold or use the thread for a limited time before it must free it again.

  Another idea, not conflicting with the one above, is to implement PROCESS in
a way that would allow it to be GC'ed automatically. Currently this does not
happen. Implementing the above idea might actually solve this "GC idea"
automatically.

|#



(defvar *current-process* nil
  "If you get a message about this being unbound ")


(defvar *global-variables* (make-hash-table :test #'eq :weakness :key)
  "Returned by VARIABLES-OF when *CURRENT-PROCSESS* is NIL.")


(declaim (inline mk-message-package))
(defun mk-message-package (message from to)
  (declare (optimize speed))
  (list message from to))
(export 'mk-message-package)


(defmacro with-message-package ((package &key (message 'message) (source 'source) (target 'target))
                                &body body)
  `(destructuring-bind (,message ,source ,target) ,package
     (declare (ignorable ,message ,source ,target))
     ,@body))
(export 'with-message-package)


(defmacro with-process (process &body body)
  `(let ((*current-process* ,process))
     (check-type *current-process* process)
     ,@body))
(export 'with-process)


(defclass process ()
  ((thread :reader thread-of)

   (incoming-messages :initform (mk-queue))
   (message-handler-ptr :initform (mk-pointer #'default-message-handler))

   (variables :reader variables-of
              :initform (make-hash-table :test #'eq :weakness :key))))
(export '(process thread-of))


(defmethod variables-of ((process (eql nil)))
  *global-variables*)


(defmethod initialize-instance :after ((process process) &key
                                       (message-handler nil message-handler-supplied-p))
  (when message-handler-supplied-p
    (setf (message-handler-of process) message-handler))
  (setf (slot-value process 'thread)
        (with-thread (process)
          (declare (optimize speed))
          ;; Attempting to make this stuff faster by not going through SLOT-VALUE-USING-CLASS on each iteration.
          (let ((incoming-messages (slot-value process 'incoming-messages))
                (message-handler-ptr (slot-value process 'message-handler-ptr)))
            (with-process process
              (with-waitqueue (process (with-locked-object incoming-messages
                                         (queue-view-next incoming-messages)))
                (with-simple-restart (process-skip-message "Skip message sent to process ~A and continue handling or waiting for messages." process)
                  (loop :for next-message = (with-locked-object incoming-messages
                                              (queue-extract-next incoming-messages))
                     :while next-message
                     :do (funcall (the function (ptr-value message-handler-ptr))
                                  next-message)))))))))


(defun send (message to &key (from *current-process*))
  "Send a MESSAGE to the process TO.
Calling this function will not block; it is async."
  (declare (process to)
           ((or process null) from)
           (optimize speed))
  (with-slots (incoming-messages) to
    (with-locked-object incoming-messages
      (queue-add incoming-messages (mk-message-package message from to)))
    (condition-notify to))
  (values))
(export 'send)


(defmethod (setf message-handler-of) ((new-fn function) (process process))
  (setf (ptr-value (slot-value process 'message-handler-ptr))
        new-fn))
(export 'message-handler-of)


(defmethod message-handler-of ((process process))
  (ptr-value (slot-value process 'message-handler-ptr)))
(export 'message-handler-of)


(defun list-all-processes ()
  (loop :for thread :in (all-threads)
     :when (typep (thread-name thread) 'process)
     :collect (thread-name thread)))
(export 'list-all-processes)


(defun process-alive-p (process)
  (declare (process process))
  (thread-alive-p (thread-of process)))
(export 'process-alive-p)


(defmethod process-terminate (process)
  (declare (process process))
  (destroy-thread (thread-of process)))
(export 'process-terminate)


(defun default-message-handler (message-package)
  (declare (optimize speed))
  (with-message-package (message-package)
    (if (functionp message)
        (funcall message message-package)
        (format t "target: ~A got message: ~A from source: ~A~%"
                target message source))))


(defun check-valid-pvar-name (sym)
  (let ((name (string sym)))
    (when (and (char= #\* (char name 0))
               (char= #\* (char name (1- (length name)))))
      (warn "Using a non-recommended naming style for \"process-variable\" ~A"
            sym))))


(defmacro defpparameter (var val &optional doc)
  "Note that variables defined by this are not special."
  (declare (ignore doc))
  (check-valid-pvar-name var)
  `(progn
     (define-symbol-macro ,var
         (values (gethash ',var (variables-of *current-process*))))
     (setf (gethash ',var (variables-of *current-process*))
           ,val)
     ',var))


(defmacro defpvar (var &optional (val nil val-supplied-p) doc)
  "Note that variables defined by this are not special."
  (declare (ignore doc))
  (check-valid-pvar-name var)
  `(progn
     (unless (boundp ',var)
       (define-symbol-macro ,var
           (values (gethash ',var (variables-of *current-process*))))
       (when val-supplied-p
         (setf (gethash ',var (variables-of *current-process*))
               ,val)))
     ',var))

    






#|
(defun test-speed ()
  (let ((process-1 (make-instance 'process))
        (process-2 (make-instance 'process
                                  :message-handler (lambda (message)
                                                     (declare (ignore message))
                                                     ))))
    (with-process process-1
      (time 
       (dotimes (i-1 100000)
         ;; PROCESS-1 ---> PROCESS-2
         (send "blah" ;;(format nil "i-1: ~A" i-1)
               process-2))))))
  



(defun test-talk-circle ()
  (let (process-1 process-2 process-3)
    (setf process-1
          (make-instance 'process 
                         :message-handler
                         (lambda (message-package)
                           (with-message-package (message-package)
                             (format t "process-1 got message ~S, forwarding it to process-2~%" message)
                             (sleep 1)
                             (send message process-2))))
          process-2
          (make-instance 'process
                         :message-handler
                         (lambda (message-package)
                           (with-message-package (message-package)
                             (format t "process-2 got message ~S, forwarding it to process-3~%" message)
                             (sleep 1)
                             (send message process-3))))
          process-3
          (make-instance 'process
                         :message-handler 
                         (lambda (message-package)
                           (with-message-package (message-package)
                             (format t "process-3 got message ~S, forwarding it to process-1~%" message)
                             (sleep 1)
                             (send message process-1)))))
    
    ;; "Bootstrap" by process-1 sending a message to process-2.
    (with-process process-1
      (send "hello!" process-2))))
|#


