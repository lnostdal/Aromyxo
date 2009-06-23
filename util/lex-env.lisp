;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)

;; from `vixey' @ #lispcafe on freenode .. cool thing
#|

(let ((x 1))
  (if (lexically-bound-p y)
      "y is here"
      "y is not here"))
=> y is not here

(let ((y 1))
  (if (lexically-bound-p y)
      "y is here"
      "y is not here"))
=> y is here
|#

(defmacro lexically-bound-p (variable &environment env)
  (eq :lexical (sb-cltl2:variable-information variable env)))
(export 'lexically-bound-p)


(defmacro symbol-macro-bound-p (variable &environment env)
  (eq :symbol-macro (sb-cltl2:variable-information variable env)))
(export 'symbol-macro-bound-p)


(defmacro type-info (name &environment env)
  (cdr (assoc 'type (third (multiple-value-list (sb-cltl2:variable-information name env))))))
(export 'type-info)


(define-symbol-macro =lex-function-name= (lex-function-name))
(defmacro lex-function-name ()
  `',(sb-c::lambda-%source-name (sb-c::lexenv-lambda sb-c:*lexenv*)))
(export '=lex-function-name=)


(define-symbol-macro =lex-variable-names= (lex-variable-names))
(defmacro lex-variable-names ()
  `',(loop :for var :in (reverse (sb-c::lexenv-vars sb-c:*lexenv*))
        :collect (car var)))
(export '=lex-variable-names=)
