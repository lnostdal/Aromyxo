;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

;; Used by both SW-STM and SW-MVC.
(eval-now
  (ignore-errors ;; In case we recompile.
    (make-dispatch-macro-character #\λ)))


(set-dispatch-macro-character #\λ #\λ
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(lambda () ,(read stream))))


(set-dispatch-macro-character #\λ #\a
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(mk-atom ,(read stream))))


(set-dispatch-macro-character #\λ #\Space
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                (assert (char= #\( (read-char stream)) nil
                                        "Correct syntax is (λ () 42), not (λ 42).")
                                (unread-char #\( stream)
                                'lambda))


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


;; Used by slots in defclass forms of classes with SELF-REF as superclass.
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


(set-dispatch-macro-character #\# #\,
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(fdefinition ',(read stream))))


(set-dispatch-macro-character #\# #\&
                              (lambda (stream char arg)
                                (declare (ignore char arg))
                                `(mk-pointer ,(read stream))))


;; Python-style multi-line strings (from http://homepages.nyu.edu/~ys453/ ).
(eval-when (:execute :load-toplevel :compile-toplevel)
  (let ((normal-string-reader (get-macro-character #\")))
    (declare (function normal-string-reader))
    (defun read-multiline-string (stream c)
      (let ((buffer ()))
        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            (funcall normal-string-reader stream c)))
        (read-char stream)

        (when (not (char= #\" (peek-char nil stream)))
          (return-from read-multiline-string
            ""))
        (read-char stream)

        (do ((chars (list (read-char stream)
                          (read-char stream)
                          (read-char stream))
                    (cdr (nconc chars (list (read-char stream))))))
            ((every (lambda (c) (eq c #\")) chars)
             (coerce (nreverse buffer) 'string))
          (push (car chars) buffer)))))

  (set-macro-character #\" #'read-multiline-string))
