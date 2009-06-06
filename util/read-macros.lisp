;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(make-dispatch-macro-character #\§)


(set-dispatch-macro-character #\# #\l
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(mk-lazy-value ,(read stream))))


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


(set-dispatch-macro-character #\§ #\c
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                (let ((form (read stream)))
                                  `(curry ',(first form) ,@(rest form)))))


(set-dispatch-macro-character #\§ #\f
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                (let ((form (read stream)))
                                  `(funcall ,(first form) ,@(rest form)))))


(set-dispatch-macro-character #\§ #\a
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                (let ((form (read stream)))
                                  `(apply ,(first form) ,@(rest form)))))
