;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


;; From Hunchentoot.
(declaim (inline escape-for-html))
(defn escape-for-html (string ((string string)))
  "Escapes the characters #\\<, #\\>, #\\', #\\\", and #\\& for HTML output."
  (declare (optimize speed))
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop :for char = (read-char in nil)
         :while char
         :do (case char
               ((#\<) (write-string "&lt;" out))
               ((#\>) (write-string "&gt;" out))
               ((#\") (write-string "&quot;" out))
               ((#\') (write-string "&#039;" out))
               ((#\&) (write-string "&amp;" out))
               (otherwise (write-char char out)))))))


(declaim (inline htmlize))
(defn htmlize (string (obj))
  (declare (optimize speed))
  (let ((*print-pretty* nil))
    (escape-for-html (if (stringp obj)
                         obj
                         (princ-to-string obj)))))
