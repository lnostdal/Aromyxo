;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro lexically-bound-p (variable &environment env)
  (eq :lexical (sb-cltl2:variable-information variable env)))
(export 'lexically-bound-p)


(defmacro symbol-macro-bound-p (variable &environment env)
  (eq :symbol-macro (sb-cltl2:variable-information variable env)))
(export 'symbol-macro-bound-p)


(defmacro lex-type-info (name &optional env)
  (once-only (name)
    (let ((env (or env sb-c:*lexenv*)))
      `(let ((sb-c:*lexenv* ,env))
         (if-let ((lambda-var (sb-c:lexenv-find ,name vars)))
           (sb-kernel:type-specifier (sb-c::lambda-var-type lambda-var))
           nil)))))
(export 'lex-type-info)


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
(defmacro lex-variable-types ()
  `(loop :for lambda-var :in ',(loop :for var :in (reverse (sb-c::lexenv-vars sb-c:*lexenv*))
                            :collect (cdr var))
      :collect (sb-kernel:type-specifier (sb-c::lambda-var-type lambda-var))))
(export '=lex-variable-types=)
