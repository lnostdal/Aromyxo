;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(defun avoid-gc (seconds &rest objects)
  (declare (integer seconds))
  ;; TODO: Better way to do this?
  (sb-ext:schedule-timer (sb-ext:make-timer (let ((objects objects)) (lambda () objects)) :thread t)
                         seconds))
(export 'avoid-gc)


(mk-meta-slot object-cache-of) ;; from object.lisp


(defun cache-object (object &key (avoid-gc nil avoid-gc-supplied-p))
  "Adds OBJECT to a simple hash-table cache.
\(ID-OF OBJECT) must yield a value unique for the type of object in question.
Returns ID and CLASS which can be passed to GET-OBJ later."
  (declare (optimize speed))
  (let ((class (class-of object))
        (id (id-of object)))
    (let ((object-cache (lazy-init (object-cache-of class)
                                   (setf (object-cache-of class)
                                         (make-hash-table :test #'equal :weakness :value :synchronized nil))
                                   (lock-of class))))
      (sb-ext:with-locked-hash-table (object-cache)
        (setf (gethash id object-cache) object))
      (when avoid-gc-supplied-p
        (avoid-gc avoid-gc object))
      (values id class))))
(export 'cache-object)


(defun get-object (id class)
  "Returns three values: OBJECT, FOUND-P and OBJECT-CACHE-FOUND-P.
CLASS can be a symbol or a CLASS type."
  (declare (optimize speed)
           ((or string rational) id)
           ((or symbol class) class))
  (when (symbolp class)
    (setf class (find-class class)))
  (multiple-value-bind (object-cache found-p)
      (with-recursive-lock-held ((lock-of class))
        (object-cache-of class))
    (if found-p
        (multiple-value-bind (obj found-p)
            (sb-ext:with-locked-hash-table (object-cache)
              (gethash id object-cache))
          (if found-p
              (values obj t t)
              (values nil nil t)))
        (values nil nil nil))))
(export 'get-object)


(defun uncache-object (object)
  "Removes OBJECT from its cache.
Returns T if it was removed or NIL if it wasn't in the cache or if no cache was
found."
  (declare (optimize speed))
  (multiple-value-bind (object-cache found-p) (object-cache-of (class-of object))
    (if found-p
        (sb-ext:with-locked-hash-table (object-cache)
          (remhash (id-of object) object-cache))
        nil)))
(export 'uncache-object)



(defclass cached-object ()
  ()

  (:documentation "
This is for extremely lazy users like me; inherit from this and you do not need
to add the created objects to \"the cache\" yourself.
You most likely want to override the ID-OF method when inheriting from this class."))
(export 'cached-object)


(defmethod initialize-instance :around ((object cached-object) &key (avoid-gc nil avoid-gc-supplied-p))
  (declare (optimize speed))
  (call-next-method)
  (multiple-value-call #'cache-object object
                       (if avoid-gc-supplied-p
                           (values :avoid-gc avoid-gc)
                           (values))))
