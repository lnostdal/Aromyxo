;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defclass sleeper ()
  (;;(name nil)
   (really-signalled-p :accessor really-signalled-p-of
                       :initform nil)

   (n-signalled :accessor n-signalled-of
                :documentation "Number of threads signalled for 'wake up'."
                :type integer
                :initform 0)

   (n-waiting :accessor n-waiting-for
              :documentation "Number of threads waiting for signal."
              :initform 0)

   (queue :accessor queue-of)
   (mutex :reader mutex-of)))


(defmethod initialize-instance :after ((sleeper sleeper) &key)
  (setf (slot-value sleeper 'mutex)
        (make-lock)
        (slot-value sleeper 'queue)
        (make-condition-variable)))


;; TODO: Add keyword :TIMEOUT.
(defmethod go-to-sleep ((sleeper sleeper))
  "Blocks *CURRENT-THREAD* until SLEEPER is \"woken up\" by calling WAKE-UP."
  (with-lock-held ((mutex-of sleeper))
    (unwind-protect
         (progn
           (incf (n-waiting-for sleeper))
           ;; Already signalled?
           (when (really-signalled-p-of sleeper)
             (return-from go-to-sleep))

           (loop
              (condition-wait (queue-of sleeper) (mutex-of sleeper))
              ;; Only return if really signalled.
              (when (really-signalled-p-of sleeper)
                (return-from go-to-sleep))))
      (setf (really-signalled-p-of sleeper) nil)
      (decf (n-waiting-for sleeper)))))


(defmethod wake-up ((sleeper sleeper))
  "Signal `sleeper' telling it to continue executing."
  (with-lock-held ((mutex-of sleeper))
    (setf (really-signalled-p-of sleeper) t)
    (condition-notify (queue-of sleeper))))



#|
SW> (defparameter *sleeper* (make-instance 'sleeper))
*SLEEPER*
SW> (with-thread 'test
      (loop
         (write-line "Sleeper going to sleep now ...") (finish-output)
         (go-to-sleep *sleeper*)
         (write-line "Sleeper woke up!") (finish-output)))
#<SB-THREAD:THREAD TEST {D0B9569}>
Sleeper going to sleep now ...
SW> (wake-up *sleeper*)
Sleeper woke up!
Sleeper going to sleep now ...
1
SW> (wake-up *sleeper*)
1
Sleeper woke up!
Sleeper going to sleep now ...
SW> (wake-up *sleeper*)
Sleeper woke up!
Sleeper going to sleep now ...
1
|#