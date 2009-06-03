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


(defun exchange (element-1 element-2 list &rest args)
  (let ((sub-1 (apply #'member element-1 list args))
        (sub-2 (apply #'member element-2 list args)))
    (setf (car sub-1) element-2
          (car sub-2) element-1))
  list)
(export 'exchange)


(defmacro place-fn (place-form)
  (with-gensyms (value value-supplied-p)
    `(lambda (&optional (,value nil ,value-supplied-p))
       (if ,value-supplied-p
           (setf ,place-form ,value)
           ,place-form))))
(export 'place-fn)


(defun insert (element list-place &key
               (after nil after-supplied-p)
               (before nil before-supplied-p)
               (last-p nil)
               (first-p nil)
               (test 'eql))
  "LIST-PLACE is created by the PLACE-FN macro or the ↺ macro character.

  AROMYXO> (let ((list (list 1 2 3 4 5)))
             (insert 3.5 ↺list :after 3)
             list)
  (1 2 3 3.5 4 5)"
  (ensure-function test)
  (ensure-function list-place)
  (let ((list (funcall list-place)))
    (cond
      (after-supplied-p
       (if-let ((tail (member after list :test test)))
         (setf (cdr tail) (cons element (cdr tail)))
         (error "~A not found in ~A." after list)))

      (before-supplied-p
       (let ((current-cons list)
             (previous-cons nil))
         (tagbody
          :again
            (if (funcall test before (car current-cons))
                (if previous-cons
                    (setf (cdr previous-cons) (cons element (cdr previous-cons)))
                    (funcall list-place (cons element list)))
                (progn
                  (setf previous-cons current-cons
                        current-cons (cdr current-cons))
                  (when current-cons
                    (go :again)))))))

      (first-p
       (funcall list-place (cons element list)))

      (last-p
       (if list
           (setf (cdr (last list)) (list element))
           (funcall list-place (list element))))

      (t
       (error ":AFTER or :BEFORE, or :FIRST-P or :LAST-P needed.")))))
(export 'insert)
