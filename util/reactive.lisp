;;;; http://nostdal.org/ ;;;;

(in-package :amu)


(defclass source ()
  ((targets :reader targets-of
            :type hash-table
            :initform (make-hash-table :test #'eq :weakness :value :synchronized t))))


(defmethod add ((target target) (source source))
  (when-let ((old-source (source-of target)))
    (remhash (id-of target) (targets-of old-source)))
  (setf (slot-value target 'source) source
        (gethash (id-of target) (targets-of source)) target)
  (sync target source))


(defmethod remove ((target target) (source source))
  (setf (slot-value target 'source) nil)
  (remhash (id-of target) (targets-of source)))



(defclass target ()
  ((source :reader source-of
           :type (or null source)
           :initform nil)))


(defmethod initialize-instance :after ((target target) &key (source nil source-supplied-p))
  (when source-supplied-p
    (add target source)))


(defmethod sync ((target target) (source source))
  (write-line "SYNC"))
