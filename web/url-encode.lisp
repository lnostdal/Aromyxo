;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)
(in-readtable aromyxo)


;; NOTE: From Hunchentoot originally.
;; TODO: An array with a fill-pointer should be faster than a string stream (I think).
(declaim (inline url-encode))
(defun url-encode (string)
  (declare (string string)
           (optimize speed (safety 0)))
  (with-output-to-string (s)
    (muffle-compiler-note
      (loop :for c character :across string
         :for index fixnum :from 0
         :do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "$-_.!*'()" :test #'char=))
                    (write-char c s))
                   (t (loop :for octet
                         :across (the (simple-array (unsigned-byte 8))
                                   (sb-ext:string-to-octets string
                                                            :start index
                                                            :end (locally (declare (optimize (safety 0)))
                                                                   (the fixnum (1+ index)))))
                         :do (format s "%~2,'0x" octet))))))))
