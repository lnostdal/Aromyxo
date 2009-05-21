(defstruct (queue (:constructor %mk-queue)
                  (:conc-name :queue-)
                  (:copier nil))
  (head nil :type list)
  (tail nil :type list))


(defun mk-queue ()
  (let ((queue (%mk-queue))
        (dummy (list t)))
    (setf (queue-head queue) dummy
          (queue-tail queue) dummy)
    queue))


(defun queue-push (queue item)
  (prog1 queue
    (let ((tail (queue-tail queue)))
      (setf (cdr tail) (list item)
            (queue-tail queue) (rest tail)))))


(defun queue-pop (queue)
  (let ((head (rest (queue-head queue))))
    (if head
        (values (prog1 (first head)
                  (setf (queue-head queue) head))
                t)
        (values nil
                nil))))
