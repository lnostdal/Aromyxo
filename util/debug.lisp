;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

(defvar *am-debug* t)


(defun debug-on ()
  (setf *am-debug* t))


(defun debug-off ()
  (setf *am-debug* nil))


(defmacro when-dbg (&body body)
  (if *am-debug*
      `(progn ,@body)
      nil))


(defmacro dbg-princ (obj &optional context-str (stream t))
  (if *am-debug*
      (with-gensyms (gobj gstream code)
        `(let ((,gobj ,obj)
               (,code ',obj)
               (,gstream ,stream))
           ,(if context-str
                `(format ,gstream "~A: ~A => ~A~%" ,context-str ,code ,gobj)
                `(format ,gstream "~A => ~A~%" ,code ,gobj))
           ,gobj))
      obj))


(defmacro dbg-prin1 (obj &optional context-str (stream t))
  (if *am-debug*
      (with-gensyms (gobj gstream code)
        `(let ((,gobj ,obj)
               (,code ',obj)
               (,gstream ,stream))
           ,(if context-str
                `(format ,gstream "~A: ~S => ~S~%" ,context-str ,code ,gobj)
                `(format ,gstream "~A => ~S~%" ,code ,gobj))
           ,gobj))
      obj))


(defun dbg-fmtf (stream ctrl-str &rest args)
  (if *am-debug*
      (apply #'format stream ctrl-str args)
      nil))


(defmacro dbg-fmt (stream ctrl-str &rest args)
  (if *am-debug*
      `(dbg-fmtf ,stream ,ctrl-str ,@args)
      (write-line "blah")))


#+swank
(progn
  (defun send-to-repl (object)
    (swank::present-repl-results (list object))
    object))
