;;;; http://nostdal.org/ ;;;;

(defpackage #:am-conc
  (:use #:cl #:cl-utilities
        #:bordeaux-threads #:sb-thread)

  
  (:shadow #:with-timeout
           #:*current-thread*
           
           #:condition-wait
           #:condition-notify
           #:condition-broadcast)
  
  
  (:shadowing-import-from #:bordeaux-threads
                          #:interrupt-thread
                          #:thread-alive-p
                          #:make-thread
                          #:join-thread
                          #:destroy-thread
                          #:thread-yield
                          #:thread-name)
                          

  (:export #:with-timeout

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
           ))

(in-package :am-conc)


