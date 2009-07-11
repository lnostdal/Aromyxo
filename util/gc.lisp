;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-variable -delay-gc-
    :kind :global
    :value (make-hash-table :test #'eq :weakness :value)
    :type hash-table)


(defun delay-gc (parent object)
  "Keep OBJECT around for at least as long as PARENT is around."
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (setf (gethash object -delay-gc-) parent)))
(export 'gc)


(defun delay-gc* (parent objects)
  "Keep OBJECTS around for at least as long as PARENT is around."
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (dolist (object objects)
      (setf (gethash object -delay-gc-) parent))))
(export 'delay-gc*)


(defmacro with-lifetime (parent &body body)
  "Top-level objects constructed in the lexical scope of BODY will be kept
around (not GCed) for at least as long as PARENT is around."
  `(delay-gc* ,parent (list ,@body)))
(export 'with-lifetime)