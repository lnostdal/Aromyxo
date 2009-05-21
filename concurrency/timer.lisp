;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defun mk-delay-callback (timeout callback &optional (timer-thread-arg t))
  "CALLBACK: Function taking 0 args.
:TIMEOUT: Seconds.
:TIMER-THREAD-ARG: Passed to :THREAD keyarg of SB-EXT:MAKE-TIMER.
Returns two values: A function and a timer.
Calling the function will call CALLBACK immediately and unschedule the call of
CALLBACK later. To never call CALLBACK at all, pass the returned timer to
SB-EXT:UNSCHEDULE-TIMER."
  (declare (fixnum timeout)
           (function callback))
  (let* ((mutex (make-lock))
         (timer (sb-ext:make-timer (lambda ()
                                     (sb-thread:with-mutex (mutex :wait-p nil)
                                       (funcall callback)))
                                   :thread timer-thread-arg)))
    (sb-ext:schedule-timer timer timeout)
    (values (lambda (&optional (execute-callback-p t))
              (sb-thread:with-mutex (mutex :wait-p nil)
                (when (sb-ext:timer-scheduled-p timer)
                  (sb-ext:unschedule-timer timer)
                  (when execute-callback-p
                    (funcall callback)))))
            timer)))
(export 'mk-delay-callback)


(defmacro with-timeout ((seconds &body on-timeout) &body body)
  (with-gensyms (the-condition)
    `(restart-case
         (handler-bind
             ((sb-ext:timeout (lambda (,the-condition)
                                (declare (ignore ,the-condition)) ;; To avoid warning.
                                ,@on-timeout)))
           (sb-ext:with-timeout ,seconds
             ,@body))
       (skip-body ()
         :report "Skip the `body'-form. (with-timeout)"))))
(export '(with-timeout skip-body))



#|
(defmacro schedule (seconds &body body)
  "Schedule `body' to run after `seconds' seconds."
  `(let ((timer (sb-ext:make-timer (lambda ()
                                    ,@body))))
     (sb-ext:schedule-timer timer ,seconds)
     timer))
(export 'schedule)


(defun unschedule (timer)
  (sb-ext:unschedule-timer timer))
(export 'unschedule)
|#                             