;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun shadowing-use-package (name &optional (package (sb-int:sane-package)))
  (let ((pkg (find-package name)))
    (do-external-symbols (sym pkg)
      (shadowing-import sym package))))


;; TODO: Describe why I do this in some blog-post or something. Short; the point -- besides me being lazy, is that conflicts can be dealt with _once_ and the result/solution trickles down ("inheritance") to sub-sub-sub..-packages.
(defun export-all (&optional (package (sb-int:sane-package)))
  (do-symbols (sym package)
    (export sym package))
  ;; Dodge CL pitfall.
  (export '(nil) package))
