;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defmethod get-hash-key (value ht &key (test #'equal) ret-when-not-found)
  (declare (hash-table ht)
           (optimize speed))
  (maphash (lambda (key hvalue)
             (when (funcall test value hvalue)
               (return-from get-hash-key (values key t))))
             ht)
  (values ret-when-not-found nil))


(defun list<-hash (hash-table)
  (declare (optimize speed))
  (collecting
    (maphash (lambda (k value) (declare (ignore k)) (collect value))
             hash-table)))
