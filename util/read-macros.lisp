;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(set-macro-character #\~
                     (lambda (stream char)
                       (declare (ignore char))
                       `(deref ,(read stream)))
                     t)


(set-macro-character #\←
                     (lambda (stream char)
                       (declare (ignore char))
                       `(deref ,(read stream)))
                     t)



#|
This adds ¤( and ¤ as meaningful constructs to the language.


For instance,

  ¤(html-element :model some-model)


..is short for,

  (make-instance 'html-element :model some-model).


¤ is used in combination with WITH-OBJECT, like this:

  (let ((view ¤(html-element :model some-model)))
    (with-object view
      ¤model))


..which would be the same as saying:

  (let ((view ¤(html-element :model some-model)))
    (with-slots (model) view
      model))
|#
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


;; TODO: Add support for the ASSERT here based on compiler-settings.
(set-macro-character #\↑
                     (lambda (stream char)
                       (declare (ignore char))
                       `(with-object (self) #|(let ((self (self)))
                                       (assert self nil "AROMYXO: While expanding the ↑ reader macro, (SELF) did not return anything.")
                                       self)|#
                          ,(read stream)))
                     t)


(set-macro-character #\↺
                     (lambda (stream char)
                       (declare (ignore char))
                       `(place-fn ,(read stream)))
                     t)




#|
 (set-macro-character #\§
                     (lambda (stream char)
                       (declare (ignore char))
                       (let* ((form (read stream)))
                         (if (char= #\( (char (princ-to-string form) 0))
                             `(make-instance ',(first form) ,@(rest form))
                             (error "AROMYXO: Error in read macro for §, expected a left paren, but got: ~A"
                                    form))))
                     t)
|#
