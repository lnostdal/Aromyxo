;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

;; On Lisp, §4.5 Mapping
;;;;;;;;;;;;;;;;;;;;;;;;

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
(export 'mapa-b)


(defun map0-n (fn n &optional (step 1))
  (mapa-b fn 0 n step))
(export 'map0-n)


(defun map1-n (fn n &optional (step 1))
  (mapa-b fn 1 n step))
(export 'map1-n)


(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))
(export 'map->)


(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))
(export 'mapcars)


(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))
(export 'rmapcar)