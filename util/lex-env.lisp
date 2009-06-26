;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(defmacro lexically-bound-p (variable &environment env)
  (eq :lexical (sb-cltl2:variable-information variable env)))
(export 'lexically-bound-p)


(defmacro symbol-macro-bound-p (variable &environment env)
  (eq :symbol-macro (sb-cltl2:variable-information variable env)))
(export 'symbol-macro-bound-p)


(defmacro lex-info (name kind &optional env)
  `(when-let* ((sb-c:*lexenv* ,(or env sb-c:*lexenv*))
               (lex-info (sb-c:lexenv-find ,name ,kind)))
     (sb-kernel:type-specifier (sb-c::leaf-type lex-info))))
(export 'lex-info)


(defmacro lex-var-info (name &optional (env nil env-supplied-p))
  (if env-supplied-p
      `(lex-info ,name vars ,env)
      `(lex-info ,name vars)))
(export 'lex-var-info)


(defmacro lex-fun-info (name &optional (env nil env-supplied-p))
  (if env-supplied-p
      `(lex-info ,name funs ,env)
      `(lex-info ,name funs)))
(export 'lex-fun-info)


;; TODO: This doesn't belong here; it extracts information about non-lexical variables.
(defun var-info (name)
  (sb-kernel:type-specifier (sb-c::info :variable :type name)))
(export 'var-info)


;; TODO: This doesn't fit here; it extracts information about non-lexical functions.
(defun fun-info (name)
  (sb-kernel:type-specifier (sb-c::info :function :type name)))
(export 'fun-info)


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
