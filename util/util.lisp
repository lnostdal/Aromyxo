;;;; http://nostdal.org/ ;;;;

(in-package aromyxo)


;; IMHO Slime/Swank should do this already, but ok. TODO: This doesn't really belong here ..
(eval-when (:execute :load-toplevel :compile-toplevel)
  (when (find-package :swank)
    (pushnew :swank *features*)))


(shadowing-import 'sb-ext:truly-the)
(export 'truly-the)


(defun generate-id-string (&optional (length 30))
  (map-into (make-sequence 'string length)
            (lambda ()
              (case (random 3)
                (0 (code-char (+ 65 (random 25))))
                (1 (code-char (+ 97 (random 25))))
                (2 (code-char (+ 48 (random 9))))
                (t (error "This shouldn't happen.."))))))
(export 'generate-id-string)


(defun relative-path (path)
  (concatenate 'string
               #+sbcl(sb-unix:posix-getcwd)
               #+clisp(ext:default-directory)
               "/" path))
(export 'relative-path)


(defun me (ex)
  "Macroexpand."
  (pprint (macroexpand-1 ex)))
(export 'me)


(defun nothing (&rest args)
  (declare (ignore args))
  nil)
(export 'nothing)


(defun true (&rest args)
  (declare (ignore args))
  t)
(export 'true)


(defun false (&rest args)
  (declare (ignore args))
  nil)
(export 'false)


(defmacro mvbind (vars value-form &body body)
  `(multiple-value-bind ,vars ,value-form ,@body))
(export 'mvbind)


(defmacro mvlist (value-form)
  `(multiple-value-list ,value-form))
(export 'mvlist)


(defmacro mvsetq (vars value-form)
  `(multiple-value-list ,vars ,value-form))
(export 'mvsetq)


(defmacro mvcall (fun &rest args)
  `(multiple-value-call ,fun ,@args))
(export 'mvcall)



#|
;; TODO: This does not work yet.
(defmacro mvsetf (places value-form)
  `(let ((vals (multiple-value-list ,value-form)))
    (list 'setf
     (dolist* ((place value) ,places vals)
                   (list place value)))))
(export 'mvsetf)
|#


(defmacro dbind (lambda-list arg-list &rest body)
  `(destructing-bind ,lambda-list ,arg-list ,@body))
(export 'dbind)


(defmacro swap (a b)
  (with-gensyms (tmp)
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))
(export 'swap)


(defmacro second-value (value-form)
  (with-gensyms (second)
    `(multiple-value-bind (%not-used ,second) ,value-form
       (declare (ignore %not-used))
       ,second)))
(export 'second-value)


(defmacro if-found (input value-symbol &body then-else)
  `(mvbind (,value-symbol found-p) ,input
    (if found-p
        (progn ,(first then-else))
        (progn ,(second then-else)))))
(export 'if-found)


(defmacro eval-now (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
(export 'eval-now)


(defmacro if-declarations (declaration-checks &body then-else)
  (let* ((active-declarations (sb-cltl2:declaration-information 'optimize))
         (compilation-speed (second (assoc 'compilation-speed active-declarations)))
         (debug (second (assoc 'debug active-declarations)))
         (safety (second (assoc 'safety active-declarations)))
         (space (second (assoc 'space active-declarations)))
         (speed (second (assoc 'speed active-declarations))))
    (declare (ignorable active-declarations compilation-speed debug safety
                        space speed))
    (if declaration-checks
        `(progn ,(first then-else))
        `(progn ,(second then-else)))))
(export 'if-declarations)


(defun mk-one-way-hash (str)
  (declare (string str))
  (apply #'concatenate 'string
         (map 'list #'princ-to-string (md5:md5sum-sequence str))))
(export 'mk-one-way-hash)


(defmacro muffle-compiler-note (&body body)
  "Use this to get rid of boring \"i-know-it-already!\" messages from SBCL."
  `(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
     ,@body))
(export 'muffle-compiler-note)


(sb-alien:define-alien-variable dynamic-space-size sb-alien:int)
(export 'dynamic-space-size)


(defmacro mk (&rest args)
  `(make-instance ,@args))
(export 'mk)


(defmacro with-continue-restart (&body body)
  `(restart-case
       (progn ,@body)
     (continue)))
(export 'with-continue-restart)


(defmacro always-continue (&body body)
  "On an ERROR condition, always look for a CONTINUE restart and dispatch to it if
found. You probably do not want to use this in normal code.."
  (with-gensyms (c)
    `(handler-bind ((error (lambda (,c)
                             (if (find-restart 'continue)
                                 (invoke-restart 'continue)
                                 (error ,c)))))
       ,@body)))
(export 'always-continue)


(defun stars (num of-possible)
  (declare (integer num of-possible))
  (with-output-to-string (ss)
    (dotimes (i num)
      (princ #\★ ss))
    (dotimes (i (- of-possible num))
      (princ #\☆ ss))))
(export 'stars)


(defun does-not-approve () "ಠ_ಠ")
(export 'does-not-approve)


(defmacro read-with-lazy-init (check read init lock)
    "* Lock-free READ.
* Lazy but safe and non-racy (LOCK) initialization via INIT."
  `(if ,check
       ,read
       (with-recursive-lock-held (,lock)
         (if ,check
             ,read
             ,init))))
(export 'read-with-lazy-init)


(defmacro lazy-init (check init lock)
  `(with ,check
     (if it it
         (with-recursive-lock-held (,lock)
           (with ,check
             (if it it
                 ,init))))))
(export 'lazy-init)
