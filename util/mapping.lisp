;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

;; On Lisp, §4.5 Mapping
;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline mapa-b))
(defun mapa-b (fn a b &optional (step 1))
  (declare (function fn)
           (number a b step))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))


(declaim (inline map0-n))
(defun map0-n (fn n &optional (step 1))
  (declare (function fn)
           (number n step))
  (mapa-b fn 0 n step))


(declaim (inline map1-n))
(defun map1-n (fn n &optional (step 1))
  (declare (function fn)
           (number n step))
  (mapa-b fn 1 n step))


(declaim (inline map->))
(defun map-> (fn start test-fn succ-fn)
  (declare (function fn test-fn succ-fn))
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))


(declaim (inline mapcars))
(defun mapcars (fn &rest lsts)
  (declare (function fn)
           (dynamic-extent lsts))
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))


(declaim (inline rmapcar))
(defun rmapcar (fn &rest args)
  (declare (function fn)
           (dynamic-extent args))
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
