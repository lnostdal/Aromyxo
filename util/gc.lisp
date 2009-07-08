;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-variable -delay-gc-
    :kind :global
    :value (make-hash-table :test #'eq :weakness :value)
    :type hash-table)


(defun delay-gc (until-gc-of-object &rest objects)
  "Keep OBJECTS around for at least as long as UNTIL-GC-OF-OBJECT is around."
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (dolist (object objects)
      (setf (gethash object -delay-gc-)
            until-gc-of-object))))
(export 'delay-gc)
