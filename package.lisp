;;;; http://nostdal.org/ ;;;;


(defpackage #:aromyxo
  (:nicknames :amx)
  (:use
   #:cl
   #:closer-mop
   #:cl-utilities #:alexandria
   #:bordeaux-threads #:sb-thread
   )


  ;; concurrency/

  (:shadow
   #:with-timeout
   #:*current-thread*

   #:condition-wait
   #:condition-notify
   #:condition-broadcast
   )

  (:shadowing-import-from #:bordeaux-threads
   #:interrupt-thread
   #:thread-alive-p
   #:make-thread
   #:join-thread
   #:destroy-thread
   #:thread-yield
   #:thread-name
   )

  (:export
   #:with-timeout

   #:thread-alive-p
   #:destroy-thread
   #:thread-name
   #:all-threads
   #:join-thread

   #:semaphore
   #:semaphore-count
   #:semaphore-name

   #:make-lock
   #:mutex
   #:make-recursive-lock
   #:with-lock-held
   #:with-recursive-lock-held

   #:acquire-lock
   #:release-lock
   )


  ;; util/

  (:shadowing-import-from #:alexandria
   #:copy-array
   #:compose
   #:with-unique-names
   #:once-only
   #:with-gensyms
   )

  (:export
   #:deref #:deref-expand
   #:full-deref
   ;#:delay-gc
   )


  ;; web/

  (:export
   #:rfc-1123-date #:iso-time
   #:url-decode #:url-encode
   #:escape-for-html)
  )



;; I think Slime/Swank should do this already, but ok.
(eval-when (:execute :load-toplevel :compile-toplevel)
  (when (find-package :swank)
    (pushnew :swank *features*)))
