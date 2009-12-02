;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(declaim (inline make-diff-timer))
(defun make-diff-timer ()
  "Returns a function that returns the diff in number of seconds between
each call to it."
  (declare (optimize speed))
  (let ((last (get-internal-real-time)))
    (declare (fixnum last))
    (lambda ()
      (let ((current (get-internal-real-time)))
        (declare (fixnum current))
        (prog1
            (- current last)
          (setf last current))))))


(defun sleep-minutes (minutes)
  "Sleep for `minutes' minutes."
  (sleep (* minutes 60)))


(defun sleep-hours (hours)
  "Sleep for `hours' hours."
  (sleep (* hours 60 60)))


(defun sleep-days (days)
  "Sleep for `days' days."
  (sleep (* days 24 60 60)))


(defun current-hour ()
  (third (mvlist (decode-universal-time (get-universal-time)))))


(defun current-day ()
  (fourth (mvlist (decode-universal-time (get-universal-time)))))


(defun current-minute ()
  (second (mvlist (decode-universal-time (get-universal-time)))))


(defun current-second ()
  (first (mvlist (decode-universal-time (get-universal-time)))))


(defun current-month ()
  (fifth (mvlist (decode-universal-time (get-universal-time)))))


(defun current-year ()
  (sixth (mvlist (decode-universal-time (get-universal-time)))))


(defmacro with-time-limiter (at-least-ms &body body)
  "If runtime for BODY hasn't taken at least AT-LEAST-MS time to finish
I will sleep the remaining time before continuing."
  (with-gensyms (start-time remaining)
    `(let ((,start-time (get-internal-real-time)))
       ,@body
       (let ((,remaining (- ,at-least-ms (- (get-internal-real-time)
                                            ,start-time ))))
         (when (plusp ,remaining)
           (sleep (/ ,remaining 1000)))))))
