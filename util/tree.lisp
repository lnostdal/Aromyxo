;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(defun map-tree (tree fn)
  (declare (list tree)
           (function fn)
           (optimize speed))
  (map nil (lambda (x)
             (if (atom x)
                 (funcall fn x)
                 (map-tree x fn)))
       tree)
  (values))
