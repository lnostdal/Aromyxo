(declaim (optimize speed))

(in-package :aromyxo)

(defstruct (queue (:constructor %mk-queue)
                  (:conc-name :queue-)
                  (:copier nil))
  (head nil :type list)
  (head-lock (make-lock "Queue head lock") :type mutex)
  (tail nil :type list)
  (tail-lock (make-lock "Queue tail lock") :type mutex))


(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :identity t :type t)
    (muffle-compiler-note
     (format stream ":HEAD ~S :TAIL ~S" (queue-head queue) (queue-tail queue)))))


(declaim (inline mk-queue))
(defun mk-queue ()
  (let ((queue (%mk-queue))
        (dummy (list t)))
    (setf (queue-head queue) dummy
          (queue-tail queue) dummy)
    queue))


(defmacro with-queue-items ((queue &key (items-sym 'items)) &body body)
  "This locks all PUSH and POP operations to QUEUE so that one might get an
isolated view of what items QUEUE contain."
  (once-only (queue)
   `(with-lock-held ((queue-head-lock ,queue))
      (with-lock-held ((queue-tail-lock ,queue))
        (let ((,items-sym (cdr (queue-head ,queue))))
          ,@body)))))


(declaim (inline queue-pop))
(defun queue-pop (queue)
  (declare (queue queue))
  "The second value returned tells whether the queue was empty or not."
  (with-lock-held ((queue-head-lock queue))
    (let ((head (cdr (queue-head queue))))
      (if head
          (values (prog1 (car head)
                    (setf (queue-head queue) head))
                  t)
          (values nil nil)))))


(declaim (inline queue-push))
(defun queue-push (queue item)
  (declare (queue queue)
           (atom item))
  (prog1 queue
    (with-lock-held ((queue-tail-lock queue))
      (let ((tail (queue-tail queue)))
        (setf (cdr tail) (list item)
              (queue-tail queue) (cdr tail))))))


(declaim (inline queue-push*))
(defun queue-push* (queue items)
  "Add ITEMS (a list of items) to QUEUE."
  (declare (queue queue)
           (list items))
  (prog1 queue
    (with-lock-held ((queue-tail-lock queue))
      (let ((tail (queue-tail queue)))
        (setf (cdr tail) items
              (queue-tail queue) (last tail))))))


(declaim (inline queue-merge))
(defun queue-merge (queue right-queue)
  (declare (queue queue right-queue))
  "Append RIGHT-QUEUE to QUEUE. This only modifies QUEUE."
  (with-queue-items (right-queue)
    (queue-push* queue items)))
