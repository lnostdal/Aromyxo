;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-variable -delay-gc-
    :value (make-hash-table :test #'eq :weakness :value)
    :type hash-table
    :doc "OBJECT -> PARENT")


(defun delay-gc (parent objects)
  "Keep OBJECTS around for at least as long as PARENTS is around.

OBJECTS can be a single object or a list of objects."
  #|(assert (eq parent parent))|#
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (dolist (object (ensure-list objects))
      #|(assert (eq object object))|#
      (multiple-value-bind (existing-parent found-p)
          (gethash object -delay-gc-)
        (if found-p
            (assert (eq existing-parent parent) nil
                    "Only a single parent or \"lifetime governor\" for ~S can exist at a time.
Currently ~S is the one assigned."
                    object existing-parent)
            (setf (gethash object -delay-gc-) parent))))))
(export 'delay-gc)


(defun cancel-delay-gc (objects &optional (parent nil parent-supplied-p))
  "Cancel out the effect created by DELAY-GC for OBJECTS."
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (if parent-supplied-p
        (dolist (object (ensure-list objects))
          (multiple-value-bind (existing-parent found-p)
              (gethash object -delay-gc-)
            (when (and found-p (eq parent existing-parent))
              (remhash object -delay-gc-))))
        (dolist (object (ensure-list objects))
          (remhash object -delay-gc-)))))
(export 'cancel-delay-gc)


(defmacro with-lifetime (parent &body body)
  "Top-level objects constructed in the lexical scope of BODY will be kept
around (not GCed) for at least as long as PARENT is around."
  `(delay-gc ,parent (list ,@body)))
(export 'with-lifetime)