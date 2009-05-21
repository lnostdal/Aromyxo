;;;; http://nostdal.org/

(in-package #:aromyxo)


(defun pad-number (number)
  "Pads number to 00 and returns it as a string."
  ;; NOTE: This currently works with \"%\" in context with SEARCH-STRING<-
  (format nil "~2,'0D" number))


(defun parse-norwegian-month (month)
  (cond
    ((string-equal month "januar") 1)
    ((string-equal month "februar") 2)
    ((string-equal month "mars") 3)
    ((string-equal month "april") 4)
    ((string-equal month "mai") 5)
    ((string-equal month "juni") 6)
    ((string-equal month "juli") 7)
    ((string-equal month "august") 8)
    ((string-equal month "september") 9)
    ((string-equal month "oktober") 10)
    ((string-equal month "november") 11)
    ((string-equal month "desember") 12)
    (t (error "~S is not a known Norwegian month." month))))
(export 'parse-norwegian-month)



(defclass date ()
  ((year :initform "%" :accessor year-of :initarg :year)
   (month :initform "%" :accessor month-of :initarg :month)
   (day :initform "%" :accessor day-of :initarg :day)))
(export '(date year-of month-of day-of))


(defmethod initialize-instance :after ((date date) &key)
  (with-slots (year month day) date
    (when (stringp year)
      (if (string= year "")
          (setf year "%")
          (unless (string= year "%")
            (setf year (parse-integer year)))))
      
    (when (stringp month)
      (if (string= month "")
          (setf month "%")
          (unless (string= month "%")
            (handler-case (setf month (parse-norwegian-month month))
              (t () (setf month (parse-integer month)))))))
      
    (when (stringp day)
      (if (string= day "")
          (setf day "%")
          (unless (string= day "%")
            (setf day (parse-integer day)))))))


(defmethod string<- ((date date))
  (format nil "~A-~A-~A"
          (year-of date)
          (pad-number (month-of date))
          (pad-number (day-of date))))
(export 'string<-)


(defmethod search-string<- ((date date))
  (format nil "~A-~A-~A"
          (year-of date)
          (pad-number (month-of date))
          (pad-number (day-of date))))
(export 'search-string<-)


(defmethod date<- ((date-string string) &key (split-character #\-) parse-as-nil)
  (let ((date (cl-utilities:split-sequence split-character date-string)))
    (if parse-as-nil
        (make-instance 'date
                       :year (when (string/= parse-as-nil (first date)) (first date))
                       :month (when (string/= parse-as-nil (second date)) (second date))
                       :day (when (string/= parse-as-nil (third date)) (third date)))
        (make-instance 'date
                       :year (first date)
                       :month (second date)
                       :day (third date)))))
(export 'date<-)


(defmethod date<- ((date-list list) &key)
  (make-instance 'date
                 :year (first date-list)
                 :month (second date-list)
                 :day (third date-list)))
(export 'date<-)


(defmethod list<- ((date date) &rest x)
  (declare (ignore x))
  (list (year-of date) (month-of date) (day-of date)))
(export 'list<-)


(defmethod norwegian-date-string-of ((date date) &optional (not-known "xx"))
  (format nil "~A. ~A ~A"
          (if-let (day (day-of date))
            day
            not-known)
          (let ((month (month-of date)))
            (case month
              (1 "Januar")
              (2 "Februar")
              (3 "Mars")
              (4 "April")
              (5 "Mai")
              (6 "Juni")
              (7 "Juli")
              (8 "August")
              (9 "September")
              (10 "Oktober")
              (11 "November")
              (12 "Desember")
              (t not-known)))
          (if-let (year (year-of date))
            year
            not-known)))
(export 'norwegian-date-string-of)
