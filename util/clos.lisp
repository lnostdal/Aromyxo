;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun really-slot-boundp (instance eslotd)
  (not (eq (standard-instance-access instance (slot-definition-location eslotd))
           sb-pcl::+slot-unbound+)))


(defmacro class-forward-reference (class-sym &body class-options)
  `(eval-now
     (unless (find-class ',class-sym nil)
       (defclass ,class-sym ()
         ()
         ,@class-options))))


(defun all-subclasses-of (class-sym &optional (include-class-sym-p t))
  "Returns a list of all subclasses of CLASS-SYM."
  (let ((all-subclasses nil))
    (labels ((inner (class)
               (dolist (sub-class (closer-mop:class-direct-subclasses class))
                 (push sub-class all-subclasses)
                 (inner sub-class))))
      (when include-class-sym-p
        (push (find-class class-sym) all-subclasses))
      (inner (find-class class-sym)))
    all-subclasses))


(defun initargs-of (class-sym)
  (flatten (mapcar #'closer-mop:slot-definition-initargs
                   (closer-mop:class-slots (find-class class-sym)))))


(defun slots-of (class-sym)
  (mapcar #'closer-mop:slot-definition-name
          (closer-mop:class-slots (find-class class-sym))))


#|
Inherit from the class SELF-REF to do things like:
 (defclass test (self-ref)
  ((a :initform (write-line (format nil "*self* is ~A~%" *self*)))))

 (defun test ()
  (let ((test (make-instance 'test)))
    (values test
            (slot-value test 'a))))

|#
(define-variable *self-refs* :value nil)

(defclass self-ref ()
  ())

;; Note that *SELF-REFS* is not visible to code in :DEFAULT-INITARGS.
(defmethod initialize-instance :around ((self-ref self-ref) &key)
  (let ((*self-refs* (cons self-ref *self-refs*)))
    (when (next-method-p)
      (call-next-method))))

(declaim (inline self))
(defun self ()
  "Returns the instance currently being constructed."
  (first *self-refs*))

#| When creating closures (SELF) will not work, but since the user might already be using the ↑ reader macro, we fall
back to the lexical binding which points to what (SELF) returned. |#
(define-symbol-macro =self=
    (if (lexically-bound-p %with-object)
        %with-object
        (self)))


(defmacro with-direct-slots-in (instance slot-value-sym &body body)
  `(dolist (,slot-value-sym (closer-mop:class-direct-slots (class-of ,instance)))
     ,@body))


(defmacro with-bound-direct-slots-in ((instance slot-name-sym &optional slot-value-sym) &body body)
  (with-gensyms (direct-slot)
    (once-only (instance)
      `(with-direct-slots-in ,instance ,direct-slot
         (let ((,slot-name-sym (closer-mop:slot-definition-name ,direct-slot)))
           (when (slot-boundp ,instance ,slot-name-sym)
             ,@(if slot-value-sym
                `((let ((,slot-value-sym (slot-value ,instance (closer-mop:slot-definition-name ,direct-slot))))
                    ,@body))
                body)))))))


(defmacro with-slots-in (instance slot-value-sym &body body)
  `(dolist (,slot-value-sym (closer-mop:class-slots (class-of ,instance)))
     ,@body))


(defmacro with-bound-slots-in ((instance slot-name-sym &optional slot-value-sym) &body body)
  (with-gensyms (slot)
    (once-only (instance)
      `(with-slots-in ,instance ,slot
         (let ((,slot-name-sym (closer-mop:slot-definition-name ,slot)))
           (when (slot-boundp ,instance ,slot-name-sym)
             ,@(if slot-value-sym
                `((let ((,slot-value-sym (slot-value ,instance ,slot-name-sym)))
                    ,@body))
                body)))))))


(defmacro with-unbound-slots-in ((instance slot-name-sym) &body body)
  (with-gensyms (slot)
    (once-only (instance)
      `(with-slots-in ,instance ,slot
         (let ((,slot-name-sym (closer-mop:slot-definition-name ,slot)))
           (unless (slot-boundp ,instance ,slot-name-sym)
             ,@body))))))



;; TODO: This should be renamed to ENSURE-SUPERCLASSES-CLASS-MIXIN (plural).

(defclass ensure-superclass-class ()
  ()
  (:documentation "
Meta-classes can inherit from this class and supply a :default-initarg for
:ensured-superclass that will mention a symbol referring to a class that is
to always be among the superclasses of instances of the class defined using
the meta-class in question."))


(defmethod initialize-instance :around ((class ensure-superclass-class) &rest initargs &key
                                        direct-superclasses
                                        (ensure-superclass nil ensure-superclass-supplied-p))
  ;;(declare (optimize speed))
  (when ensure-superclass-supplied-p
    (when-let ((ensure-superclass (find-class ensure-superclass nil)))
      (if (some (lambda (class) (subtypep class ensure-superclass))
                direct-superclasses)
          (call-next-method)
          (apply #'call-next-method class
                 :direct-superclasses (push ensure-superclass direct-superclasses)
                 initargs)))))


(defmethod reinitialize-instance :around ((class ensure-superclass-class) &rest initargs &key
                                          (direct-superclasses nil direct-superclasses-supplied-p)
                                          (ensure-superclass nil ensure-superclass-supplied-p))
  ;;(declare (optimize speed))
  (when ensure-superclass-supplied-p
    (if direct-superclasses-supplied-p
        (when-let ((ensure-superclass (find-class ensure-superclass nil)))
          (if (some (lambda (class) (subtypep class ensure-superclass))
                    direct-superclasses)
              (call-next-method)
              (apply #'call-next-method class
                     :direct-superclasses (push ensure-superclass direct-superclasses)
                     initargs)))
        (call-next-method))))
