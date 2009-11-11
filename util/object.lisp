;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(defmacro mk-meta-slot (accessor-name &key
                        (test '#'eq) (weakness :key) synchronized (type t)
                        documentation
                        default)
  (with-gensyms (slot-data)
    `(let ((,slot-data (make-hash-table :test ,test
                                        :weakness ,weakness
                                        :synchronized ,synchronized)))

       (defmethod (setf ,accessor-name) (new-value (object ,type))
         ,documentation
         (setf (gethash object ,slot-data) new-value))

       (defmethod ,accessor-name ((object ,type))
         ,documentation
         (gethash object ,slot-data ,default))

       (defmethod slot-data-of ((accessor-name (eql ',accessor-name)))
         ,slot-data))))
(export '(mk-meta-slot object slot-data-of))


(declaim (inline mk-id-generator)
         (ftype (function () (values (cons integer mutex) &optional))
                mk-id-generator))
(defun mk-id-generator ()
  (declare (optimize speed))
  (cons 0 (make-lock)))
(export 'mk-id-generator)


(declaim (inline id-generator-next)
         (ftype (function ((cons integer mutex)) (values integer &optional))
                id-generator-next))
(defun id-generator-next (id-generator)
  (declare (optimize speed))
  (with-lock-held ((cdr id-generator))
    (muffle-compiler-note
      (incf (car id-generator)))))
(export 'id-generator-next)


(declaim (inline id-generator-next-str)
         (ftype (function ((cons integer mutex)) (values string &optional))
                id-generator-next-str))
(defun id-generator-next-str (id-generator)
  (declare (optimize speed))
  (write-to-string (id-generator-next id-generator)
                   :pretty nil
                   :escape nil
                   :base 36))
(export 'id-generator-next-str)



(define-variable -id-generator-
    :kind :global
    :value (mk-id-generator))
(export '-id-generator-)


(defclass id-mixin ()
  ((id :reader id-of
       :type string)))
(export '(id-mixin id id-of))


(defmethod initialize-instance :before ((obj id-mixin) &key (id nil id-supplied-p))
  (declare (optimize speed))
  (setf (slot-value obj 'id)
        (if id-supplied-p
            id
            (id-generator-next-str -id-generator-)
            ;; TODO: This adds a bit more overhead wrt. bandwidth than I'm comfortable with, but it's still nice
            ;; wrt. debugging. As always it sucks adding yet another run-time check type of thing...
            ;; Perhaps debugging should be done by using the TITLE attribute?
            #|(catstr (string (type-of obj)) "-" (id-generator-next-str -id-generator-))|#)))


(defmethod print-slots progn ((id-mixin id-mixin) stream)
  (when (slot-boundp id-mixin 'id)
    (format stream " :ID ~S" (id-of id-mixin))))
