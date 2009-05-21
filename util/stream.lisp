;;;; http://nostdal.org/ ;;;;

(in-package :am-util)

(defun readString (stream num)
  "`num' is number of characters to read."
  (let ((seq (make-array num)))
    (read-sequence seq stream)
    (coerce seq 'string)))
(export 'readString)
