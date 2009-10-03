;;;; http://nostdal.org/ ;;;;


(defsystem aromyxo
  :description "Messy collection of utilities and hacks. A scratchpad."
  :license "AGPLv3 + GPL linking exception."
  :author "Lars Rune Nøstdal <larsnostdal@gmail.com> http://nostdal.ath.cx/"

  :depends-on (;;:named-readtables
               :closer-mop
               :sb-cltl2
               :cl-utilities :alexandria
               :md5
               :bordeaux-threads
               :cl-ppcre)

  :serial t
  :components
  ((:file "package")
   (:module util
            :serial t
            :components
            ((:file "util") ;; TODO: This also contains some code. Should probably move stuff around.
             (:file "read-macros")
             (:file "types")
             (:file "define-variable")
             (:file "lex-env")
             (:file "printing")
             (:file "deref") ;; TODO: This actually contains the code for the WITH-OBJECT macro.
             (:file "parse-number")

             (:file "add-to")
             (:file "metadata")
             (:file "symbols")
             (:file "fflet")
             (:file "random")
             (:file "hash")
             (:file "anaphoric")
             (:file "date")
             (:file "numbers")
             (:file "sequences")
             (:file "context")
             (:file "function-builders")
             (:file "conditional-evaluation")
             (:file "generalized-variables")
             (:file "iteration")
             (:file "lists")
             (:file "queue")
             (:file "debug")
             (:file "string")
             (:file "mapping")
             (:file "time")
             (:file "clos")
             (:file "clos-mutex-slot-mixin")
             ;;(:file "clos-serialized-object-mixin")
             ;;(:file "url") ;; TODO: Maybe just delete this thing.
             (:file "object")
             (:file "cached-object")
             (:file "gc")
             (:file "pointer")
             #|(:file "connection")|#
             (:file "defun")
             ))


   (:module concurrency
            :serial t
            :components
            (;;(:file "concurrency") ;; defpackage stuff from before the merge of am-util and am-conc
             (:file "thread")
             ;;(:file "sleeper")
             (:file "mutex")
             (:file "timer")
             (:file "condition") ;; TODO: rename to waitqueue.lisp ..?
             (:file "locked-object")
             (:file "semaphores")
             (:file "process")
             (:file "atomic")
             ))

   (:module web
            :serial t
            :components
            ((:file "constants")
             (:file "time")
             (:file "escaping")
             (:file "url-decode")
             (:file "url-encode")
             ))
   ))
