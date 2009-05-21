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
                       (let ((form (read stream)))
                         (if (char= #\( (char (princ-to-string form) 0))
                             `(make-instance ',(first form) ,@(rest form))
                             `(slot-value %with-object ',form))))
                     t)







#|(set-macro-character #\↺
                     (lambda (stream char)
                       (declare (ignore char))
                       `(with-model-of ,(read stream))))|#


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