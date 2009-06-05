;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro retryable (&body body)
  (with-gensyms (retry-tag block-name)
    `(block ,block-name
       (tagbody
          ,retry-tag
          (restart-case
              (return-from ,block-name (progn  ,@body))
            (retry ()
              :report (lambda (stream) (format stream "retry"))
              (go ,retry-tag)))))))
(export 'retryable)


(defmacro macrolet* (definitions &body body)
  (if (null definitions)
      `(progn ,@body)
      `(macrolet (,(first definitions))
         (macrolet* ,(rest definitions)
           ,@body))))
(export 'macrolet*)


(defmacro with-nth ((&rest names) list
                    &body body)
  (once-only (list)
    `(symbol-macrolet ,(loop :for index :from 0 :to (length names)
                             :for name :in names
                          :collect `(,name (nth ,index ,list)))
       ,@body)))
(export 'with-nth)


(defmacro letp1 (bindings &body body)
  "First value of BINDINGS is returned."
  `(let ,bindings
     (prog1 ,(caar bindings)
       ,@body)))
(export 'letp1)
