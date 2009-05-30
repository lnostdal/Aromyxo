;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(define-global +lisp-obj->metadata+
    (make-hash-table :test #'equal)) ;; I'm not sure EQ has what it takes wrt. "range" here.


(defun lisp-obj-set-metadata (lisp-obj key value)
  (let ((hash +lisp-obj->metadata+)
        (signature (sb-kernel:get-lisp-obj-address lisp-obj)))
    (sb-ext:with-locked-hash-table (hash)
      (multiple-value-bind (key_value-hash found-p)
          (gethash signature hash)
        (if found-p
            (sb-ext:with-locked-hash-table (key_value-hash)
              (setf (gethash key key_value-hash) value))
            (progn
              (setf (gethash key (setf (gethash signature hash) (make-hash-table :test #'eq)))
                    value)
              (sb-ext:finalize lisp-obj
                               (lambda ()
                                 (remhash signature hash)))))))))
(export 'lisp-obj-set-metadata)


(defun lisp-obj-get-metadata (lisp-obj key)
  (let ((hash +lisp-obj->metadata+)
        (signature (sb-kernel:get-lisp-obj-address lisp-obj)))
    (sb-ext:with-locked-hash-table (hash)
      (multiple-value-bind (key_value-hash found-p)
          (gethash signature hash)
        (if found-p
            (sb-ext:with-locked-hash-table (key_value-hash)
              (gethash key key_value-hash))
            (values nil nil))))))
(export 'lisp-obj-get-metadata)
                