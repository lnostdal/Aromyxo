;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)


(defreadtable aromyxo
  (:merge :standard)

  (:macro-char #\λ :dispatch)
  (:dispatch-macro-char #\λ #\λ
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(lambda () ,(read stream))))

  (:dispatch-macro-char #\λ #\A
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-atom ,(read stream))))

  (:dispatch-macro-char #\λ #\P
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            `(mk-ptr ,(read stream))))

  (:dispatch-macro-char #\λ #\Space
                        #'(lambda (stream char arg)
                            (declare (ignore char arg))
                            (assert (char= #\( (read-char stream)) nil
                                    "Correct syntax is (λ () 42), not (λ 42).")
                            (unread-char #\( stream)
                            'lambda))

  (:macro-char #\~ #'(lambda (stream char)
                       (declare (ignore char))
                       `(deref ,(read stream)))
               t)

  (:macro-char #\↑ #'(lambda (stream char)
                       (declare (ignore char))
                       `(with-object (self)
                          ,(read stream)))
               t)

  (:macro-char #\¤ #'(lambda (stream char)
                       (declare (ignore char))
                       `(slot-value %with-object ',(read stream)))
               t)

  (:macro-char #\↺ #'(lambda (stream char)
                       (declare (ignore char))
                       `(place-fn ,(read stream)))
               t)

  (:dispatch-macro-char #\# #\&
                        (lambda (stream char arg)
                          (declare (ignore char arg))
                          `(mk-ptr ,(read stream)))))
