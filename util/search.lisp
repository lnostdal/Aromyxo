;; http://nostdal.org/ ;;;;

(in-package :am-util)


;; On Lisp: §4.4 Search
;;;;;;;;;;;;;;;;;;;;;;;


(defun find2 (fn lst)
  "TODO: Example for this."
  (if (null lst)
      nil
      (let ((val (funcall fn (first lst))))
        (if val
            (values (first lst) val)
            (find2 fn (rest lst))))))
(export 'find2)


