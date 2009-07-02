;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(set-macro-character #\~
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((form (read stream)))
                         (if (and (listp form) (listp (first form)))
                             `(funcall ,(first form) ,@(rest form))
                             `(deref ,form))))
                     t)


(set-macro-character #\←
                     (lambda (stream char)
                       (declare (ignore char))
                       `(deref ,(read stream)))
                     t)


(set-macro-character #\¤
                     (lambda (stream char)
                       (declare (ignore char))
                       (let* ((form (read stream))
                              (first-char (char (princ-to-string form) 0)))

                         (cond
                           ((char= #\( first-char)
                            `(make-instance ',(first form) ,@(rest form)))

                           (t
                            `(slot-value %with-object ',form)))))
                     t)


(set-macro-character #\↑
                     (lambda (stream char)
                       (declare (ignore char))
                       `(with-object (self)
                          ,(read stream)))
                     t)


(set-macro-character #\↺
                     (lambda (stream char)
                       (declare (ignore char))
                       `(place-fn ,(read stream)))
                     t)
