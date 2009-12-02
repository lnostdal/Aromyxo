;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)


(defgeneric add-to* (target items))


(defmethod add-to-compiler-source-caar (target source-body source-caar)
  nil)


(defmethod add-to-compiler (target source-body)
  (cond
    ((listp (car source-body))
     (add-to-compiler-source-caar target source-body (caar source-body)))

    (t
     nil)))


(defmacro add-to (target &body source-body)
  (if-let (expansion (add-to-compiler target source-body))
    expansion

    ;; Fall back to this method if no compile-time expansion was found.
    `(add-to* ,target (list ,@source-body))))
