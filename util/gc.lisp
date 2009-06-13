;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-global -delay-gc-
    (make-hash-table :test #'eq :weakness :value))


(defun delay-gc (until-gc-of-object &rest objects)
  "Keep OBJECTS around for at least as long as UNTIL-GC-OF-OBJECT is around."
  (sb-ext:with-locked-hash-table (-delay-gc-)
    (dolist (object objects)
      (setf (gethash object -delay-gc-)
            until-gc-of-object))))
(export 'delay-gc)
