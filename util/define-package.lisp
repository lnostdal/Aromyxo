;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defun %define-package-mk (name &optional nicknames)
  (unless (find-package name)
    (make-package name :use (list) :nicknames nicknames)))


(defun shadowing-use-package (name)
  (let ((pkg (find-package name)))
    (do-external-symbols (sym pkg)
      (shadowing-import sym))))


(defmacro define-package (name &key use nicknames)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (%define-package-mk ,name ,nicknames)
       ;; Handle Common Lisp pitfall; (import 'nil) or (export 'nil) will not work!
       (shadowing-import '(cl:nil) (find-package ,name)))

     ,@(loop :for pkg :in use
          :collect `(eval-when (:compile-toplevel :load-toplevel :execute)
                      (do-external-symbols (sym (find-package ,pkg))
                        (shadowing-import sym (find-package ,name)))))))
