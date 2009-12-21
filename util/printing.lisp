;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defclass printed-slots ()
  ())


(defgeneric print-slots (object stream)
  (:method-combination progn))


(defmethod print-object :around ((object printed-slots) stream)
  "Override this to control whether the type and identity of OBJECT should be printed."
  (print-unreadable-object (object stream :type t :identity t)
    (call-next-method)))


(defmethod print-object ((object printed-slots) stream)
  (if (compute-applicable-methods #'print-slots (list object stream))
      ;; User wants to control which slots are printed, and how.
      (print-slots object stream)
      ;; Just print all bound slots.
      (let ((values nil))
        (dolist (dslotd (sb-mop:class-slots (class-of object))
                 (setf values (nreverse values)))
          (let ((slot-name (sb-mop:slot-definition-name dslotd)))
            (when (slot-boundp object slot-name)
              (push (cons slot-name (slot-value object slot-name))
                    values))))
        (format stream "~{~A~^ ~}" values))))