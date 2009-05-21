;;;; http://nostdal.org/ ;;;;


(in-package #:aromyxo)

#|
TODO
----

Less locking. I wish I had more time to improve this ..

  * READ  -> READ

  * WRITE -> WRITE

  * WRITE -> READ

  * READ  -> WRITE


  Add tests to "prove" that this thing will work (or not work?) under all
circumstances. Maybe use a state machine simulation to do this?

  The "test -maybe-> lock --> re-test -maybe-> set/delete" pattern seems to
happen a lot. Add some macro for this?

  Implement the :ON-COMMIT and :ON-ABORT parameters for WITH-SYNC.
|#


;;(declaim (optimize speed (debug 0) (safety 0)))
(declaim (optimize speed))


(export
 '(;;*current-transaction*
   transaction tr-id tr-name tr-on-commit-fns tr-on-abort-fns
   with-sync
   when-commit
   get-memory set-memory
   stm-class
   ref
   ref-value-of
   mk-ref
   with-refs))


(defparameter *current-transaction*
  nil)


#| TODO: It would probably make sense to have these on a pr. PROCESS basis
so the user could add new processes to increase concurrency.

Or, uh, let's just implement a lock-free hash-table (for SBCL)! Or, don't store
_anything_ in hash-tables at all; let the memory-locations themselves have an
extra slot that tell whether they are free or taken. Lock-free assignment
of these slots is possible via sb-ext:compare-and-swap.

NOTE: The reason there are two "conflict resolution hash-tables" like this is
because the SLOT-VALUE-USING-CLASS methods are not passed unique (EQ), on a
pr. instance basis, designators for slots and thus we need to use
\(cons instance slot-definition) to designate unique memory location
type signatures. This might be useful in other contexts also. |#
(define-global +eq-signature->transaction+
    (make-hash-table :test #'eq
                     :synchronized t))


(define-global +equal-signature->transaction+
    (make-hash-table :test #'equal
                     :synchronized t))


(define-global +transaction-counter+
    (mk-id-generator))



(defstruct (transaction (:constructor mk-transaction (&key name on-abort-fns on-commit-fns))
                        (:conc-name :tr-)
                        (:copier nil))
  (id (id-generator-next +transaction-counter+) :type integer)
  (name nil)
  
  (lock (make-lock) :type mutex)
  (condition (sb-thread:make-waitqueue) :type sb-thread:waitqueue)

  (on-abort-fns nil :type list)
  (on-commit-fns nil :type list)
  ;;(nest-p nil)

  #| Has another transaction asked us to abort? This slot will refer to the
  transaction which caused us to abort, if so. Note that SIGNALLED-P should
  be checked even if a transaction is marked as ABORTED-P. |#
  (aborted-p nil)

  #| Even if a transaction is ABORTED-P, it might already have been sent a
  wake-up call. |#
  (signalled-p nil)
  
  (completed-p nil)
  
  (tries 0 :type fixnum)
  
  ;; ((SIGNATURE . HASH-TABLE) ..)
  (signatures nil :type list)
  
  ;; [SIGNATURE -> VALUE]
  (eq-object-data (make-hash-table :test #'eq :synchronized nil)
                  :type hash-table)
  (equal-object-data (make-hash-table :test #'equal :synchronized nil)
                     :type hash-table))


(declaim (inline conflict-hash-of))
(defun conflict-hash-of (signature-type)
  (declare (symbol signature-type))
  (case signature-type
    (:eq +eq-signature->transaction+)
    (:equal +equal-signature->transaction+)
    (otherwise (error "CONFLICT-HASH-OF: Unknown signature-type: ~A" signature-type))))


(declaim (inline object-data-of))
(defun object-data-of (transaction signature-type)
  (declare (transaction transaction)
           (symbol signature-type))
  (case signature-type
    (:eq (tr-eq-object-data transaction))
    (:equal (tr-equal-object-data transaction))
    (otherwise (error "OBJECT-DATA-OF: Unknown signature type: ~A" signature-type))))


(declaim (inline transaction-num-resources-held))
(defun transaction-num-resources-held (transaction)
  (declare (transaction transaction))
  (+ (hash-table-count (tr-eq-object-data transaction))
     (hash-table-count (tr-equal-object-data transaction))))


(declaim (inline signature-touched-p))
(defun signature-touched-p (signature transaction conflict-hash)
  "Returns two values:

  * NIL, :US or :SOMEONE-ELSE
  * SOMEONE-ELSE (the transaction already touching SIGNATURE)

NOTE: CONFLICT-HASH is not locked by this function."
  (declare (transaction transaction)
           (hash-table conflict-hash))
  (let ((existing-transaction (gethash signature conflict-hash)))
    (if existing-transaction
        (if (eq existing-transaction transaction)
            :us
            (values :someone-else existing-transaction))
        nil)))


(declaim (inline signature-touch))
(defun signature-touch (signature transaction conflict-hash)
  "This will replace any existing transaction which may hold SIGNATURE.

NOTE: CONFLICT-HASH is not locked by this function."
  (declare (transaction transaction)
           (hash-table conflict-hash))
  (push (cons signature conflict-hash) (tr-signatures transaction))
  (setf (gethash signature conflict-hash)
        transaction))


;;(declaim (inline signature-attempt-touch))
(defun signature-attempt-touch (signature transaction conflict-hash)
  "Either returns NIL when TRANSACTION got access,
or returns the transaction instance that did or already has.

NOTE: CONFLICT-HASH is locked by this function under some circumstances."
  (declare (transaction transaction)
           (hash-table conflict-hash))
  (multiple-value-bind (touched-by existing-transaction)
      (signature-touched-p signature transaction conflict-hash)
    (case touched-by
      ((nil)
       ;; Lock and test again before, maybe, setting.
       (sb-ext:with-locked-hash-table (conflict-hash)
         (multiple-value-bind (touched-by existing-transaction)
             (signature-touched-p signature transaction conflict-hash)
           (case touched-by
             ((nil)
              (signature-touch signature transaction conflict-hash)
              nil)
      
             (:someone-else
              existing-transaction)
             
             (:us
              nil)))))

      (:someone-else
       existing-transaction)

      (:us
       nil))))


#| TODO: This thing is messy. The nested with-lock-held stuff should probably be
a macro or something. The `(let ((result ..)))' and `return-from' stuff is messy
also. |#
;;(declaim (inline signature-resolve-conflict))
(defun transaction-resolve-conflict (us them signature conflict-hash)
  (declare (transaction us them)
           (hash-table conflict-hash))
  ;; This will never change back, so we get on with things ASAP. TODO: Place this earlier in the call stack?
  (when (tr-completed-p us)
    (return-from transaction-resolve-conflict t))
  (prog1 t ;; Return T if no re-check is needed.
    (let ((their-id (tr-id them))
          (our-id (tr-id us)))
      (with-lock-held ((if (> their-id our-id)
                           (tr-lock us)
                           (tr-lock them)))
        (with-lock-held ((if (> their-id our-id)
                             (tr-lock them)
                             (tr-lock us)))
          (let ((our-tries (tr-tries us))
                (our-resources (transaction-num-resources-held us))
                (their-tries (tr-tries them))
                (their-resources (transaction-num-resources-held them)))
            (let ((result
                   (cond
                     ;; After getting the lock, some other "resolver" has decided that we are the ones to be aborted.
                     ((tr-aborted-p us)
                      (throw 'transaction :abort))

                     ;; After getting the lock, the other transaction we where in conflict with has already finished.
                     ;; We return NIL to signal that we're interested in an attempt to re-acquire the resource.
                     ((tr-completed-p them)
                      #+:stm-debug
                      (warn "## TRANSACTION-RESOLVE-CONFLICT: (COMPLETED-P-OF THEM) => T (US: ~A THEM: ~A)"
                            (tr-name us) (tr-name them))
                      ;; TODO: We return at once without waiting. Maybe add an user-controllable (optional) pause?
                      (return-from transaction-resolve-conflict nil))
                     
                     ;; After getting the lock, the other transaction we where in conflict with has been aborted.
                     ;; We return NIL to signal that we're interested in an attempt to re-acquire the resource.
                     ((tr-aborted-p them)
                      #+:stm-debug
                      (warn "## TRANSACTION-RESOLVE-CONFLICT: (ABORTED-P-OF THEM) => T (US: ~A THEM: ~A)"
                            (tr-name us)
                            (tr-name them))
                      (multiple-value-bind (other-transaction found-p)
                          (gethash signature conflict-hash)
                        (when (and found-p (eq other-transaction them))
                          ;; Dodge "EPIC FAIL" bug. This happens when THEM is marked as ABORTED-P by another
                          ;; transaction while THEM is in or about to enter SIGNATURE-ATTEMPT-TOUCH.
                          ;; Note that it should be safe to do REMHASH here because we are holding a lock on THEM
                          ;; and the WHEN test is an atomic "focus point" here.
                          (remhash signature conflict-hash)))
                      ;; TODO: We return at once without waiting. Maybe add an user-controllable (optional) pause?
                      (return-from transaction-resolve-conflict nil))
                     
                     ((> our-tries their-tries)
                      :them)
                     
                     ((< our-tries their-tries)
                      :us)
                     
                     (t
                      (cond
                        ((> our-resources their-resources)
                         :them)
                        
                        ((< our-resources their-resources)
                         :us)
                        
                        (t
                         (if (oddp (random 2)) ;; TODO: Is the ID slot something which could be used also?
                             :them
                             :us)))))))

              #+:stm-debug
              (format t "TRANSACTION-RESOLVE-CONFLICT: ~A (T: ~A R: ~A) vs. ~A (T: ~A R: ~A) => ~A must wait~%"
                      (tr-name us) our-tries our-resources
                      (tr-name them) their-tries their-resources
                      (case result
                        (:us (tr-name us))
                        (:them (tr-name them))))

              (case result
                (:us
                 ;; We'll never return from this method anyway, so we do not care about return-value here.
                 (transaction-abort us them))
                
                (:them
                 (transaction-abort them us)
                 ;; We return NIL to signal that we're interested in an attempt to re-acquire the resource.
                 (return-from transaction-resolve-conflict nil))))))))))


(flet ((ensure-no-conflicts (signature transaction conflict-hash)
         (tagbody
          :again
            (when-let ((other-transaction (signature-attempt-touch signature transaction conflict-hash)))
              (unless (transaction-resolve-conflict transaction other-transaction signature conflict-hash)
                (go :again))))))
  (declare (inline ensure-no-conflicts))

  
  (defun set-memory (signature new-value real-set-fn signature-type)
    (declare (function real-set-fn)
             (symbol signature-type))
    (if-let ((transaction *current-transaction*))
      (progn
        (when (tr-aborted-p transaction)
          (throw 'transaction :abort))
        (ensure-no-conflicts signature transaction (conflict-hash-of signature-type))
        ;; There is no race between this 'setter' and other 'getters'
        ;; because ENSURE-NO-CONFLICTS prevents this.
        (car (setf (gethash signature (object-data-of transaction signature-type))
                   (cons new-value real-set-fn))))
      (funcall real-set-fn)))


  (defun get-memory (signature real-get-fn signature-type)
    (declare (function real-get-fn)
             (symbol signature-type))
    (if-let ((transaction *current-transaction*))
      (progn
        (when (tr-aborted-p transaction)
          (throw 'transaction :abort))
        (ensure-no-conflicts signature transaction (conflict-hash-of signature-type))
        ;; There is no race between the check and the set here because
        ;; ENSURE-NO-CONFLICTS prevents this.
        (let ((object-data (object-data-of transaction signature-type)))
          (multiple-value-bind (value found-p)
              (gethash signature object-data)
            (if found-p
                (car value)
                (car (setf (gethash signature object-data)
                           (cons (funcall real-get-fn) real-get-fn)))))))
      (funcall real-get-fn))))


;;(declaim (inline transaction-free-resources))
(defun transaction-free-resources (transaction)
  "Free resources locked or held by TRANSACTION.
Note that this does the minimum amount of work possible; it only \"marks\"
resources as free for others to take, and does not \"clean up\" any
temporary data used or stored in TRANSACTION itself."
  (declare (transaction transaction))
  ;; We need to lock because the situation might change between the EQ and the
  ;; REMHASH call. This is the only place where we lock both of these
  ;; hash-tables "at the same time".
  (sb-ext:with-locked-hash-table (+eq-signature->transaction+)
    (sb-ext:with-locked-hash-table (+equal-signature->transaction+)
      (dolist (signature (tr-signatures transaction)) ;; (SIGNATURE . HASH-TABLE)
        (when (eq transaction (gethash (car signature) (cdr signature)))
          (remhash (car signature) (cdr signature)))))))


;;(declaim (inline transaction-commit))
(defun transaction-commit (transaction)
  (declare (transaction transaction))
  (maphash (lambda (signature value_real-set-fn)
             (declare (ignore signature)
                      (cons value_real-set-fn))
             (funcall (the function (cdr value_real-set-fn))))
           (tr-eq-object-data transaction))
  (maphash (lambda (signature value_real-set-fn)
             (declare (ignore signature)
                      (cons value_real-set-fn))
             (funcall (the function (cdr value_real-set-fn))))
           (tr-equal-object-data transaction)))


(defmacro when-commit ((&rest args) &body body)
  "BODY will be executed when the transaction commits.
Keep in mind that BODY is stored as a closure; you might need to set up bindings
before the WHEN-COMMIT form or use ARGS to do it for you."
  `(flet ((body-fn () ,@body))
     (if *current-transaction*
         (let (,@(loop :for arg :in args :collect `(,arg ,arg)))
           (push #'body-fn (tr-on-commit-fns *current-transaction*)))
         (progn
           ;; TODO: Think about this.
           ;; This situation tends to happen while constructing objects. Add an :AROUND INITIALIZE-INSTANCE
           ;; method, or something?
           ;; (warn "WHEN-COMMIT: Not inside the dynamic scope of a WITH-SYNC form?")
           (funcall #'body-fn)))))


(defmacro with-sync ((&rest args) &body body)
  "  ARGS: [refs] &key name before on-commit on-abort

  :NAME is be the name of the transaction; this is useful when debugging.

  :ON-COMMIT is code which is to be executed when the transaction has finished
and is about to commit. As long as the code here does not \"touch\" shared
variables that have not already been \"touched\" by the transaction (us), these
will be executed only once and one can place code that have permanent
side-effects (I/O) here. Also see WHEN-COMMIT. TODO: :ON-COMMIT isn't fully
implemented at the moment.

  :ON-ABORT is code which is to be executed when the transaction aborts or rolls
back. TODO: :ON-ABORT isn't fully implemented atm.

  :NEST-P determines whether transactions should be nestable or whether any
nesting will cause a \"sub-transaction\" to be merged with its \"parent\".
The default is NIL."
  `(with-refs (,@(when (listp (first args)) (first args)))
     (transaction-begin (list ,@(if (listp (first args))
                                    (rest args)
                                    args))
                        (lambda ()
                          (block nil ,@body)))))


(defun transaction-begin (keyargs fn)
  (declare (list keyargs)
           (function fn))
  (let ((return-values nil)
        (*current-transaction* (if *current-transaction*
                                   (return-from transaction-begin (funcall fn))
                                   (apply #'mk-transaction keyargs))))
    (declare (transaction *current-transaction*))
    (loop :do
       (with-slots (name lock condition tries aborted-p completed-p signalled-p) *current-transaction*
         (declare (fixnum tries)
                  (mutex lock))
         (unwind-protect-case ()
           (catch 'transaction
             (setf return-values (multiple-value-list (funcall fn)))
             (with-lock-held (lock)
               (unless aborted-p
                 (tf completed-p) ;; At this point we do not want anyone to abort us.
                 (dolist (on-commit-fn (reverse (tr-on-commit-fns *current-transaction*)))
                   (funcall (the function on-commit-fn)) transaction)
                 #+:stm-debug (format t "~A finished!~%" name)
                 ;; Start the process which will make permanent changes to data.
                 (transaction-commit *current-transaction*)
                 (transaction-free-resources *current-transaction*)
                 (tf signalled-p) 
                 (sb-thread:condition-broadcast condition))))
           
           (:normal
            (unless completed-p
              #+:stm-debug (format t "~A aborted!~%" name)
              ;; We've been aborted. Wait for the one who caused us to abort to finish.
              (with-slots (lock condition name signalled-p) aborted-p ;; The transaction we're waiting for.
                (with-lock-held (lock)
                  (loop
                     :while (not signalled-p)
                     :do (progn
                           #+:stm-debug (format t "~A waiting for ~A~%" (tr-name *current-transaction*) name)
                           (sb-thread:condition-wait condition lock)
                           #+:stm-debug (format t "~A woke up!~%" (tr-name *current-transaction*))
                           )
                     :while (not signalled-p))))
              ;; Doing this is ok because the resources have already been freed in TRANSACTION-ABORT.
              (with-lock-held (lock)
                (nilf aborted-p
                      (tr-signatures *current-transaction*)
                      (tr-on-commit-fns *current-transaction*))
                (clrhash (tr-eq-object-data *current-transaction*))
                (clrhash (tr-equal-object-data *current-transaction*))
                (incf tries))))
           
           (:abort
            ;; TODO: The code here kinda overlaps with the code in TRANSACTION-ABORT and vice-versa ..
            (with-lock-held (lock)
              (unwind-protect
                   (progn
                     (tf aborted-p) (nilf completed-p)
                     (transaction-free-resources *current-transaction*)
                     (dolist (on-abort-fn (tr-on-abort-fns *current-transaction*))
                       (funcall (the function on-abort-fn) *current-transaction*)))
                (tf signalled-p)
                (condition-broadcast condition))))))
       :while (not (tr-completed-p *current-transaction*)))
    (values-list return-values)))

       
(defun transaction-abort (transaction other-transaction)
  (declare (transaction transaction other-transaction))
  "Abort TRANSACTION because of conflict with OTHER-TRANSACTION.
This will call the ON-ABORT-FNS of TRANSACTION before actually aborting."
  (unwind-protect
       (dolist (on-abort-fn (tr-on-abort-fns transaction))
         (funcall (the function on-abort-fn) transaction))
    (with-slots (condition aborted-p completed-p) transaction
      #+:stm-debug (assert (not aborted-p))
      #+:stm-debug (assert (not completed-p))
      (setf aborted-p other-transaction)
      (transaction-free-resources transaction)
      (when (eq transaction *current-transaction*)
        (throw 'transaction :abort)))))



(defstruct (ref (:constructor mk-ref (&optional value))
                (:conc-name :%ref-)
                (:copier nil))
  "A transactional reference or pointer to something; some value.
Changing the reference is done in a transactional fashion."
  (value nil))


(defun (setf ref-value-of) (new-value ref)
  (declare (ref ref))
  (set-memory ref
              new-value
              (lambda () (setf (%ref-value ref) new-value))
              :eq))


(defun ref-value-of (ref)
  (declare (ref ref))
  (get-memory ref
              (lambda () (%ref-value ref))
              :eq))


;; TODO: Implement compiler macro that dispatch to REF-VALUE-OF directly, if possible?
(defmethod deref ((ref ref))
  (ref-value-of ref))


(defmethod (setf deref) (new-value (ref ref))
  (setf (ref-value-of ref) new-value))


(defmacro with-refs ((&rest refs) &body body)
  "Convenience macro. Instead of saying (REF-VALUE-OF A) or (SETF (REF-VALUE-OF A) 1)
one can instead say A and (SETF A 1)."
  (with-gensyms (mrefs)
    `(let ((,mrefs (vector ,@refs)))
       (declare (dynamic-extent ,mrefs)
                (ignorable ,mrefs))
       (symbol-macrolet (,@(loop :for ref :in refs
                                 :for index :from 0
                              :collect `(,ref (ref-value-of (svref ,mrefs ,index)))))
         ,@body))))



(defclass stm-class (standard-class)
  ()
  (:documentation "
This metaclass ensures that writing and reading (set/get) of class slots is
done in a transactional fashion when initiated within the dynamic scope of a
WITH-SYNC form."))


(defmethod validate-superclass ((class stm-class) (superclass standard-class))
  t)


(defmethod (setf slot-value-using-class) :around (new-value (class stm-class) instance slot-definition)
  (set-memory (cons instance slot-definition)
              new-value
              (lambda () (call-next-method))
              :equal))


(defmethod slot-value-using-class :around ((class stm-class) instance slot-definition)
  (get-memory (cons instance slot-definition)
              (lambda () (call-next-method))
              :equal))



#| TODO: This thing needs more work.  Hm, or maybe all this is a bad idea.
It might be better to create data structures that are tailored for STM and
STM only. That way, there is no way for the user to mess with stuff in a
non-transactional fashion unless he _really_ means it (extract internal data
structures and manipulate them directly). |#
(defmacro define-stm-wrapper (accessor-name real-name args &key (signature (first args)))
  `(progn
     (defun (setf ,accessor-name) (new-value ,@args)
       (set-memory ,signature
                   new-value
                   (lambda () (setf (,real-name ,@args) new-value))
                   :eq))

     (defun ,accessor-name (,@args)
       (get-memory ,signature
                   (lambda () (,real-name ,@args))
                   :eq))))


;; TODO: The idea here is to locally shadow stuff in the CL package,
;; and create "forwarders" to the TCAR etc. functions.
(defmacro with-stm-wrappers ((&rest args) &body body)
  `(progn ,@body))


(define-stm-wrapper tcar car (cons))
(define-stm-wrapper tcdr cdr (cons))
(define-stm-wrapper trest rest (cons))
;; TODO: CAAR, CADR, CDAR, CDDR, CAAAR, CAADR, CADAR, CADDR, CDAAR, CDADR, CDDAR, CDDDR, CAAAAR, CAAADR, CAADAR, CAADDR, CADAAR, CADADR, CADDAR, CADDDR, CDAAAR, CDAADR, CDADAR, CDADDR, CDDAAR, CDDADR, CDDDAR, CDDDDR
;; TODO: FIRST -> TENTH
(define-stm-wrapper tnth nth (n list) :signature list)
(define-stm-wrapper telt elt (sequence index))
(define-stm-wrapper tchar char (string index))
(define-stm-wrapper tschar schar (string index))



#| NOTE: This isn't needed and reduces concurrency quite a bit anyway.
(defun signature-untouch (signature transaction)
  "Untouch SIGNATURE, but only if it is being held by TRANSACTION.
NOTE: The back-end hash-table is locked while this is done."
  (declare (transaction transaction))
  (sb-ext:with-locked-hash-table (*signature->transaction*)
    (multiple-value-bind (existing-transaction found-p)
        (gethash signature *signature->transaction*)
      (when (and found-p (eq transaction existing-transaction))
        (remhash signature *signature->transaction*)))))
|#
