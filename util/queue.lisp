;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed))


;; TODO: Consider switching to SB-QUEUE, but for now a QUEUE-PEEK function is missing there.

(export '(queue
          queue-head queue-head-lock
          queue-tail queue-tail-lock
          mk-queue
          with-queue-items items
          queue-peek
          queue-pop
          queue-push
          queue-push*
          queue-append))


(defstruct (queue (:constructor %mk-queue)
                  (:conc-name :queue-)
                  (:copier nil))
  (head nil :type list)
  (head-lock (make-lock "QUEUE head-lock") :type mutex)
  (tail nil :type list)
  (tail-lock (make-lock "QUEUE tail-lock") :type mutex))


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
  "This locks all PUSH and POP operations to QUEUE for the duration of BODY so
that one might get an isolated view of what items QUEUE contain."
  (once-only (queue)
   `(with-lock-held ((queue-head-lock ,queue))
      (with-lock-held ((queue-tail-lock ,queue))
        (let ((,items-sym (rest (queue-head ,queue))))
          ,@body)))))


(declaim (inline queue-pop))
(defun queue-pop (queue)
  (declare (queue queue))
  "The second value returned tells whether the queue was empty or not."
  (with-lock-held ((queue-head-lock queue))
    (let ((head (rest (queue-head queue))))
      (if head
          (values (prog1 (first head)
                    (setf (queue-head queue) head))
                  t)
          (values nil
                  nil)))))


(declaim (inline queue-peek))
(defun queue-peek (queue)
  (declare (queue queue))
  (with-lock-held ((queue-head-lock queue))
    (if-let (head (first (rest (queue-head queue))))
      (values head t)
      (values nil nil))))


(declaim (inline queue-push))
(defun queue-push (queue item)
  (declare (queue queue)
           (atom item))
  (prog1 queue
    (with-lock-held ((queue-tail-lock queue))
      (let ((tail (queue-tail queue)))
        (setf (cdr tail) (list item)
              (queue-tail queue) (rest tail))))))


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


(declaim (inline queue-append))
(defun queue-append (queue right-queue)
  (declare (queue queue right-queue))
  "Append RIGHT-QUEUE to QUEUE. This modifies QUEUE."
  (with-queue-items (right-queue)
    (queue-push* queue items)))













#|
;; TODO: Add some tests to this thing!


;;; Dumb queue thingy
;;;;;;;;;;;;;;;;;;;;;


(deftype queue () 'cons)
(export 'queue)

(defconstant +queue-head+ '%queue-head)


;; TODO: DEFINE-COMPILER-MACRO or check if SBCL is smart enough to determine when
;; INITIAL-ELEMENTS is constant and thus always NIL.
;;(declaim (inline mk-queue))
(defun mk-queue (&rest initial-elements)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (list initial-elements))
  (if initial-elements
      (let ((head (push +queue-head+ initial-elements)))
        (cons head (last head)))
      (let ((head (cons +queue-head+ nil)))
        (cons head head))))
(export 'mk-queue)


(declaim (inline queue-add))
(defun queue-add (queue element)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (queue queue))
  (let ((new-elt (cons element nil)))
    (setf (cddr queue) new-elt
          (cdr queue) new-elt)
    (values)))
(export 'queue-add)


(declaim (inline queue-extract-next))
(defun queue-extract-next (queue)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (queue queue))
  (unless (eq +queue-head+ (cdr queue))
    (prog1 (pop (cdar queue))
      (unless (cdar queue)
        (setf (cdr queue) (car queue))))))
(export 'queue-extract-next)


(declaim (inline queue-view-next))
(defun queue-view-next (queue)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (queue queue))
  (cadar queue))
(export 'queue-view-next)


#|
(declaim (inline queue-remove))
(defun queue-remove (queue element)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (queue queue))
  (deletef (cdar queue) element))
|#


(declaim (inline queue-as-list))
(defun queue-as-list (queue)
  (declare (optimize (speed 3) (space 0) (safety 0) (compilation-speed 0))
           (queue queue))
  (cdar queue))
(export 'queue-as-list)


;; TODO: I don't have time to do this proper ... :/
(defun queue-merge (queue other-queue)
  "Append OTHER-QUEUE to end of QUEUE."
  (dolist (elt (queue-as-list other-queue))
    (queue-add queue elt)))
#|
  (if (cdar queue)
      (setf (cdr (cdar queue))
            (cdar other-queue))
      (setf (cdar queue)
            (cdar other-queue))))
|#
(export 'queue-merge)

|#