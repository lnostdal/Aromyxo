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
