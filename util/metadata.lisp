;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=
(declaim (optimize speed (safety 0)))


;; [LISP-OBJ -> [KEY -> VALUE]]
(define-variable +lisp-obj->metadata+
    :kind :global
    :value (make-hash-table :test #'eq :weakness :key :synchronized t)
    :type hash-table)


(defun set-metadata (lisp-obj key value)
  (let ((hash +lisp-obj->metadata+))
    (sb-ext:with-locked-hash-table (hash)
      (multiple-value-bind (key_value-hash found-p)
          (gethash lisp-obj hash)
        (if found-p
            (sb-ext:with-locked-hash-table (key_value-hash)
              (setf (gethash key key_value-hash) value))
            (setf (gethash key (setf (gethash lisp-obj hash) (make-hash-table :test #'eq)))
                  value))))))


;; This ensures that NOT-FOUND-VALUE will only be evaluated once.
(defmacro metadata-of (lisp-obj key &optional (not-found-value nil not-found-value-supplied-p))
  (with-gensyms (key->value value found-p)
    `(sb-ext:with-locked-hash-table (+lisp-obj->metadata+)
       (let ((,key->value (multiple-value-bind (,key->value ,found-p)
                              (gethash ,lisp-obj +lisp-obj->metadata+)
                            (if ,found-p
                                ,key->value
                                (setf (gethash ,lisp-obj +lisp-obj->metadata+)
                                      (make-hash-table :test #'eq))))))
         (sb-ext:with-locked-hash-table (,key->value)
           (multiple-value-bind (,value ,found-p)
               (gethash ,key ,key->value)
             (if ,found-p
                 (values ,value t)
                 ,(if not-found-value-supplied-p
                      `(values (setf (gethash ,key ,key->value) ,not-found-value) :added)
                      `(values nil nil)))))))))


(defsetf metadata-of set-metadata)
