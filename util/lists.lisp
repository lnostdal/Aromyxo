;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro extract-option (option options &optional default-value)
  "Destructively extract `option' from plist `options'."
  `(aif option (find ,option ,options :key #'first)
     (prog1
         (second option)
       (setf ,options (delete (first option) ,options :key #'first)))
     ,default-value))
(export 'extract-option)


(defmacro last1 (lst)
  "Example:
\(last  '(a b c)) => (c)
\(last1 '(a b c)) => c"
  `(car (last ,lst)))
(export 'last1)


(declaim (inline mklst))
(defun mklst (obj)
  "Example:
\(mklist 1) => (1)
\(mklist (list 1)) => (1)"
  (if (listp obj)
      obj
      (list obj)))
(export 'mklst)


(defun group (n source)
  "Example:
\(group '(a b c d e f) 2) => ((A B) (C D) (E F))
\(group '(a b c d e f) 3) => ((A B C) (D E F))"
  (if (zerop n) (error "Zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
(export 'group)

#|
(defun insert (element position list)
  "Inserts an `element' at `position' in `list', and returns the result."
  `(,@(subseq list 0 position)
    ,element
    ,@(subseq list position)))
(export 'insert)
|#


(defun exchange (element-1 element-2 list &rest args)
  (let ((sub-1 (apply #'member element-1 list args))
        (sub-2 (apply #'member element-2 list args)))
    (setf (car sub-1) element-2
          (car sub-2) element-1))
  list)
(export 'exchange)
