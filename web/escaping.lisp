;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


;; From Hunchentoot.
(defun escape-for-html (string)
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (declare (optimize speed)
           (string string))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop :for char = (read-char in nil nil)
         :while char
         :do (case char
               ((#\<) (write-string "&lt;" out))
               ((#\>) (write-string "&gt;" out))
               ((#\") (write-string "&quot;" out))
               ((#\') (write-string "&#039;" out))
               ((#\&) (write-string "&amp;" out))
               (otherwise (write-char char out)))))))
