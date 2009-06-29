;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(eval-when (:compile-toplevel :load-toplevel)
  (defvar *lexenvs*
    (make-hash-table :test #'equal :weakness :value)))


;; This will enable us to extract the LEXENV as late as possible; when the
;; structure has been fully built.
(eval-when (:compile-toplevel :load-toplevel)
  (defmethod make-load-form ((obj sb-kernel:lexenv) &optional env)
    (declare (ignore env))
    (let ((addr (sb-kernel:get-lisp-obj-address obj)))
      (setf (gethash addr *lexenvs*) obj)
      `(gethash ,addr *lexenvs*))))


(defmacro lex-info (&environment %env name kind &optional (env %env))
  `(when-let* ((sb-c:*lexenv* ,env)
               (lex-info (sb-c:lexenv-find ,name ,kind)))
     (sb-kernel:type-specifier (sb-c::leaf-type lex-info))))
(export 'lex-info)


(defmacro lex-var-info (&environment %env name &optional (env %env))
  `(lex-info ,name vars ,env))
(export 'lex-var-info)


(defmacro lex-fun-info (&environment %env name &optional (env %env))
  `(lex-info ,name funs ,env))
(export 'lex-fun-info)


(define-symbol-macro =lex-function-name= (lex-function-name))
(defmacro lex-function-name ()
  `',(sb-c::lambda-%source-name (sb-c::lexenv-lambda sb-c:*lexenv*)))
(export '=lex-function-name=)


(define-symbol-macro =lex-variable-names= (lex-variable-names))
(defmacro lex-variable-names ()
  `',(loop :for var :in (reverse (sb-c::lexenv-vars sb-c:*lexenv*))
        :collect (car var)))
(export '=lex-variable-names=)


(define-symbol-macro =lex-variable-types= (lex-variable-types))
(defmacro lex-variable-types (&environment %env &optional (env %env))
  `(loop :for lambda-var :in ',(loop :for var :in (reverse (sb-c::lexenv-vars env))
                                  :collect (cdr var))
      :collect (sb-kernel:type-specifier (sb-c::lambda-var-type lambda-var))))
(export '=lex-variable-types=)


(defmacro lexically-bound-p (variable &environment env)
  (eq :lexical (sb-cltl2:variable-information variable env)))
(export 'lexically-bound-p)


(defmacro symbol-macro-bound-p (variable &environment env)
  (eq :symbol-macro (sb-cltl2:variable-information variable env)))
(export 'symbol-macro-bound-p)


;; TODO: This doesn't belong here; it extracts information about non-lexical variables.
(defun var-info (name)
  (sb-kernel:type-specifier (sb-c::info :variable :type name)))
(export 'var-info)


;; TODO: This doesn't fit here; it extracts information about non-lexical functions.
(defun fun-info (name)
  (sb-kernel:type-specifier (sb-c::info :function :type name)))
(export 'fun-info)
