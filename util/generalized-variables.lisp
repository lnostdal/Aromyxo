;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(defmacro allf (val &rest places)
  "SETF all PLACES to VAL. VAL is only evaluated once.
See ALLF* for a version that evaluates VAL multiple times if PLACES > 1."
  (with-gensyms (mval)
    (if (= 1 (length places))
        `(setf ,@places ,val)
        `(let ((,mval ,val))
           (setf ,(first places) ,mval)
           (allf ,mval ,@(rest places))))))


(defmacro allf* (val &rest places)
  "SETF all PLACES to VAL. VAL is evaluated multiple times if PLACES is > 1.
See ALLF for a version that will never evaluate VAL more than once."
  (if (= 1 (length places))
      `(setf ,@places ,val)
      `(progn
         (setf ,(first places) ,val)
         (allf* ,val ,@(rest places)))))


(defmacro nilf (&rest args)
  "Set all ARGS to NIL."
  `(allf nil ,@args))


(defmacro tf (&rest args)
  "Set all ARGS to T."
  `(allf t ,@args))
