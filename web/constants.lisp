;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(define-variable +cr+
    :kind :constant
    :value (format nil "~C" #\Return)
    :test #'string=)
(export '+cr+)


(define-variable +lf+
    :kind :constant
    :value (format nil "~C" #\Newline)
    :test #'string=)
(export '+lf+)

(define-variable +crlf+
    :kind :constant
    :value (format nil "~C~C" #\Return #\Newline)
    :test #'string=)
(export '+crlf+)


(define-constant +day-names+
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  :test #'equalp)


(define-constant +month-names+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  :test #'equalp)
