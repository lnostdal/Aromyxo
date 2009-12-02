;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun pos-of-nth (nth item sequence &key (test #'eql) (start 0) end key (from-end nil))
  "Returns position of `nth' (0 is first item) `item' in `sequence'."
  (labels ((ipos-of-nth (nth last-pos)
             (let ((pos (position item sequence :test test :start last-pos :end end :key key :from-end from-end)))
               (if (= 0 nth)
                   pos
                   (ipos-of-nth (1- nth) (1+ pos))))))
    (ipos-of-nth nth start)))


(declaim (inline mk-array-view))
(defun mk-array-view (array &key (start 0) (end (length array)))
  (declare (fixnum start end)
           (optimize speed))
  (make-array (- end start)
              :element-type (array-element-type array)
              :displaced-to array
              :displaced-index-offset start))
