;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


;; NOTE: From Hunchentoot originally.
(declaim (inline url-decode))
(defun url-decode (string)
  (declare (string string)
           (optimize speed))
  (let ((vector (make-array (length string)
                            :element-type '(unsigned-byte 8)
                            :fill-pointer 0)))
    (muffle-compiler-note
      (loop :with percent-p :and buff
         :for char :of-type character :across string
         :for i :of-type fixnum :from 0
         :when buff
         :do (vector-push (parse-integer string
                                         :start (1- i)
                                         :end (1+ i)
                                         :radix 16)
                          vector)
         (setq buff nil)
         :else :when percent-p
         :do (setq buff t
                   percent-p nil)
         :else :when (char= char #\%)
         :do (setq percent-p t)
         :else :do (vector-push (char-code (case char
                                             ((#\+) #\Space)
                                             (otherwise char)))
                                vector))
      (sb-ext:octets-to-string vector))))
