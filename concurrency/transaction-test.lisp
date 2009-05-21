;;;; http://nostdal.org/ ;;;;

(in-package #:aromyxo)


(declaim (optimize speed))


(defun test ()
  (let* ((a (mk-ref 0))
         (b (mk-ref 0))
         (thread (with-thread (nil)
                   (with-sync ((a b) :name "thread-a")
                     (incf a)
                     (sleep 0.5)
                     (incf b)
                     (assert (= a b))))))

    (with-sync ((a b) :name "thread-b")
      (incf a)
      (sleep 1)
      (incf b)
      (assert (= a b)))

    (join-thread thread)

    (with-refs (a b)
      (assert (= a b 2)))))




(defun test2 (&key (num-threads 3) (max-sleep 20))
  (let* ((a (mk-ref 0))
         (b (mk-ref 0))
         (c (mk-ref 0))
         (threads (collecting
                   (dotimes (i num-threads)
                     (let ((i i))
                       (collect
                        (with-thread ((format nil "thread-~A" i))
                          (when (plusp max-sleep)
                            (sleep (/ (random max-sleep) 10)))
                          (with-sync ((a b c) :name (format nil "thread-trans-~A" i))
                            (let ((oprs (list (lambda ()
                                                #+:stm-debug
                                                (dbg-princ (incf a) (fmtn "trans-o-~A" i))
                                                #-:stm-debug
                                                (incf a))
                                              (lambda ()
                                                #+:stm-debug
                                                (dbg-princ (incf b) (fmtn "trans-o-~A" i))
                                                #-:stm-debug
                                                (incf b))
                                              (lambda ()
                                                #+:stm-debug
                                                (dbg-princ (incf c) (fmtn "trans-o-~A" i))
                                                #-:stm-debug
                                                (incf c)))))
                              (dotimes (i 3)
                                (let ((opr (random-elt oprs)))
                                  (setf oprs (deletef oprs opr))
                                  (funcall opr)
                                  (when (plusp max-sleep)
                                    (sleep (/ (random max-sleep) 10)))
                                  )))

                            (assert (apply #'=
                                           #+:stm-debug
                                           (dbg-princ (list a b c) (fmtn "~A (assert)" i))
                                           #-:stm-debug
                                           (list a b c)
                                           ))))))))))


    (with-refs (a b c)
      (loop :while (some #'thread-alive-p threads)
         :do (sleep 0.5))
      (assert (apply #'=  (list num-threads a b c))))));;(dbg-princ (list num-threads a b c)))))))




(defclass person ()
  ((first-name :accessor first-name-of :initarg :first-name
               :initform "")
   (last-name :accessor last-name :initarg :last-name
              :initform ""))
  
  (:metaclass stm-class))


(defun test-class ()
  (let ((person (make-instance 'person
                               :first-name "lars"
                               :last-name "nøstdal")))
    (with-slots (first-name last-name) person
      (let ((thread (with-thread (nil)
                      (with-sync ()
                        (setf first-name "donald")
                        (sleep 0.5)
                        (setf last-name "duck")
                        (assert (and (string= first-name "donald")
                                     (string= last-name "duck"))
                                nil
                                "assert ~AA ~A failed.." first-name last-name)))))

        (with-sync ()
          (setf first-name "petter")
          (sleep 1)
          (setf last-name "smart")
          (assert (and (string= first-name "petter")
                       (string= last-name "smart"))))

        (join-thread thread)
        
        (assert (or (and (string= first-name "donald")
                         (string= last-name "duck"))
                    (and (string= first-name "petter")
                         (string= last-name "smart"))))))))

      


(defun test-starvation ()
  (let* ((keep-running-p t)
         (a (mk-ref 0)))

    (with-thread (nil)
      (loop :while keep-running-p
         :do (with-sync ((a) :name "thread-a")
               (let ((old-a a))
                 (sleep 1)
                 (incf a)
                 (let ((current-a a))
                   (assert (= current-a
                              (1+ old-a)))
                   (format t "thread-a: ~A~%" current-a))))))
    
    (with-thread (nil)
      (loop :while keep-running-p
         :do (with-sync ((a) :name "thread-b")
               (let ((old-a a))
                 (sleep 0.25)
                 (incf a)
                 (let ((current-a a))
                   (assert (= current-a
                              (1+ old-a)))
                   (format t "thread-b: ~A~%" current-a))))))
    
    (sleep 10)
    (setf keep-running-p nil)
    (ref-value-of a)))




(defun test-unithread (&optional (fail-p t))
  (let ((a (mk-ref 1))
        (b (mk-ref 1)))

    (ignore-errors
      (with-sync ((a b))
        (incf a)
        (when fail-p (error "Adding 1 to B fails for some reason."))
        (incf b)))
           
    (with-refs (a b)
      (dbg-princ a)
      (dbg-princ b)
      (assert (= a b
                 (if fail-p
                     1
                     2))))))



(defun test-deadlock ()
  (let ((a (mk-ref 2))
        (b (mk-ref 3))
        (c (mk-ref 4)))
    
    (with-thread (nil)
      (with-sync ((a b c) :name "first")
        (format t "a - b - c = ~A~%"
                (- a
                   (prog1 0 (sleep 1))
                   b
                   (prog1 0 (sleep 1))
                   c))))

    (with-thread (nil)
      (with-sync ((a b c) :name "second")
        (format t "c - a - b = ~A~%"
                (- c
                   (prog1 0 (sleep 0.75))
                   a
                   (prog1 0 (sleep 0.75))
                   b))))
    
    (with-sync ((a b c) :name "third")
      (format t "b - c - a = ~A~%"
              (- b
                 (prog1 0 (sleep 0.5))
                 c
                 (prog1 0 (sleep 0.5))
                 a)))))





