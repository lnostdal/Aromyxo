;;;; http://nostdal.org/ ;;;;

(in-package :am-util)

(defun find-hex (string &optional (start-at-pos 0))
  (position #\% string :start start-at-pos))


(defun hex-value (string &optional (at-pos 0))
  "(hex-value \"%0A\" 0) => 10"
  (read-from-string (format nil "#X~A" (subseq string (+ at-pos 1) (+ at-pos 3)))))


(defun value-hex (code)
  ;; TODO: (value-hex 10) returns "%A", not "%0A" - I'm not sure if it matters.
  (format nil "%~X" code))


(defun url-decode (url)
  "Returns an url-decoded string. TODO: Currently assumes we are working with UTF-8.
Example: (AmWeb:urlDecode \"n%C3%B8stdal\") => \"nøstdal\""
  ;; Convert + to space.
  (setf url (substitute #\Space #\+ url))
  (do* ((last-pos 0 (+ curr-pos 1)) ;; Advance only one step, since it's a single character after conversion.
        (curr-pos (find-hex url last-pos) (find-hex url last-pos)))
       ((not curr-pos) url)
    (let ((hex-value (hex-value url curr-pos)))
      (if (< hex-value 128)
          ;; Read one.
          (setf url (exchange-with (string (code-char hex-value))
                                  url curr-pos 3))
          (if (< hex-value 224)
              ;; Read two.
              (setf url
                    (exchange-with (string (code-char (+ (* (mod hex-value 32) 64)
                                                        (mod (hex-value url (+ curr-pos 3)) 64))))
                                  url curr-pos 6))
              ;; Read three.
              (setf url
                    (exchange-with (string (code-char (+ (* (mod hex-value 16) 4096)
                                                        (* (mod (hex-value url (+ curr-pos 3)) 64) 64)
                                                        (mod (hex-value url (+ curr-pos 6)) 64))))
                                  url curr-pos 9)))))))


(defun url-encode (str)
  "Example: (AmWeb:urlEncode \"nøstdal\") => \"n%C3%B8stdal\""
  (let ((res ""))
    (map nil (lambda (ch)
               (cond
                 ((> 128 (char-code ch))
                  (setf res (mkstr res ch)))
                 ((> 2048 (char-code ch))
                  (let ((val (char-code ch)))
                    (setf res (mkstr res (value-hex (+ 192 (/ (- val (mod val 64))
                                                             64)))))
                    (setf res (mkstr res (value-hex (+ 128 (mod val 64)))))))
                 (t
                  (let ((val (char-code ch)))
                    (setf res (mkstr res (value-hex (+ 224 (/ (- val (mod val 4096))
                                                             4096)))))
                    (setf res (mkstr res (value-hex (+ 128 (/ (- (mod val 4096) (mod val 64))
                                                             64)))))
                    (setf res (mkstr res (value-hex (+ 128 (mod val 64)))))))))
         str)
    res))


(defun url-split (str)
  "Returns an associative list of key/value -pairs."
  (let ((alist nil))
    (let ((hash (cl-ppcre:split "&" str)))
      (dolist (e hash)
        (let ((key_value (cl-ppcre:split "=" e)))
          (setf alist
                (acons (url-decode (first key_value))
                       (url-decode (second key_value))
                       alist)))))
    (reverse alist)))
