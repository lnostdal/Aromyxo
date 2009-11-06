;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


(define-variable +cr+
    :value (format nil "~C" #\Return)
    :test #'string=)
(export '+cr+)


(define-variable +lf+
    :value (format nil "~C" #\Newline)
    :test #'string=)
(export '+lf+)

(define-variable +crlf+
    :value (format nil "~C~C" #\Return #\Newline)
    :test #'string=)
(export '+crlf+)


(define-variable +day-names+
    :value #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
    :test #'equalp)
(export '+day-names+)


(define-variable +month-names+
    :value #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
    :test #'equalp)
(export '+month-names+)
