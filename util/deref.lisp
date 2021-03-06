;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defmacro with-object (object &body body)
  `(let ((%with-object ,object))
     (declare (ignorable %with-object))
     ,@body))


(defgeneric mderef (arg))
(defgeneric (setf mderef) (new-value arg))


#| The following weird'ish code is here because methods in SBCL won't currently dispatch at compile-time, but plain
TYPECASE can. ADD-DEREF-TYPE is used to "add new methods" vs. DEREF. |#


(define-variable -deref-typecase*-
    :value (list))


(defun add-deref-type (type-symbol &key
                       get-expansion set-expansion
                       (replace-existing-p t))
  "If SET-EXPANSION is EQ to T, a simple, default expansion based on GET-EXPANSION will be created."
  (when replace-existing-p
    (deletef -deref-typecase*- type-symbol
             :key #'first :test #'eq))
  (pushnew `(,type-symbol (,(funcall get-expansion '%arg)
                           ,(with set-expansion
                              (if (eq it t)
                                  `(setf ,(funcall get-expansion '%arg) %new-value)
                                  (when (functionp it)
                                    (funcall it '%arg '%new-value))))))
           -deref-typecase*-
           :key #'first :test #'eq)
  (eval
   `(progn
      (declaim (inline deref))
      (defun deref (%arg)
        (declare (optimize speed (safety 0))
                 (sb-ext:muffle-conditions style-warning)) ;; Perhaps typed CELLs is needed?
        (typecase %arg
          ,@(mapcar (λ (tc) `(,(first tc) ,(caadr tc)))
                    -deref-typecase*-)
          ;; Fall back to CLOS method dispatch.
          (t (mderef %arg))))

      (declaim (inline (setf deref)))
      (defun (setf deref) (%new-value %arg)
        (declare (optimize speed (safety 0))
                 (sb-ext:muffle-conditions style-warning))
        (typecase %arg
          ,@(mapcar (lambda (cs)
                      `(,(first cs) ,(with (cadadr cs)
                                           (if (eq it t)
                                               `(setf ,(caadr cs) %new-value)
                                               it))))
                    (remove-if (λ (tc) (eq nil (cadadr tc)))
                               -deref-typecase*-))
          ;; Fall back to CLOS method dispatch.
          (t (setf (mderef %arg) %new-value)))))))


(add-deref-type 'function
                :get-expansion (λ (arg-sym) `(funcall ,arg-sym))
                :set-expansion (λ (arg-sym new-value-sym) `(funcall ,arg-sym ,new-value-sym)))


(add-deref-type 'list
                :get-expansion (λ (arg-sym) `(mapcar (lambda (elt)
                                                       (declare (notinline deref))
                                                       (deref elt))
                                                     ,arg-sym)))

(add-deref-type 'sb-ext:weak-pointer
                :get-expansion (lambda (arg-sym)
                                 `(sb-ext:weak-pointer-value ,arg-sym)))
