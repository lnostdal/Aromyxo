;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


;; From Hunchentoot.
(defun rfc-1123-date (&optional (time (get-universal-time)))
  (declare (optimize speed)
           (unsigned-byte time))
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref +day-names+ day-of-week)
            date
            (svref +month-names+ (1- month))
            year
            hour
            minute
            second)))


;; From Hunchentoot.
(defun iso-time (&optional (time (get-universal-time)))
  (declare (optimize speed))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))
