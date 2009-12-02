;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun shadowing-use-package (name)
  (let ((pkg (find-package name)))
    (do-external-symbols (sym pkg)
      (shadowing-import sym))))
