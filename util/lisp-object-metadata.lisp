;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(declaim (optimize speed (safety 0)))


(define-global +lisp-obj->metadata+ 
    (make-hash-table :test #'eq :weakness :key))


(defun lisp-obj-set-metadata (lisp-obj key value)
  (let ((hash +lisp-obj->metadata+))
    (sb-ext:with-locked-hash-table (hash)
      (multiple-value-bind (key_value-hash found-p)
          (gethash lisp-obj hash)
        (if found-p
            (sb-ext:with-locked-hash-table (key_value-hash)
              (setf (gethash key key_value-hash) value))
            (setf (gethash key (setf (gethash lisp-obj hash) (make-hash-table :test #'eq)))
                  value))))))
(export 'lisp-obj-set-metadata)


(defun lisp-obj-get-metadata (lisp-obj key)
  (let ((hash +lisp-obj->metadata+))
    (sb-ext:with-locked-hash-table (hash)
      (multiple-value-bind (key_value-hash found-p)
          (gethash lisp-obj hash)
        (if found-p
            (sb-ext:with-locked-hash-table (key_value-hash)
              (gethash key key_value-hash))
            (values nil nil))))))
(export 'lisp-obj-get-metadata)


(defun (setf metadata-of) (value lisp-obj key)
  (lisp-obj-set-metadata lisp-obj key value))
(export 'metadata-of)


(defun metadata-of (lisp-obj key)
  (lisp-obj-get-metadata lisp-obj key))
(export 'metadata-of)
  