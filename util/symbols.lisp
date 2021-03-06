;;;; http://nostdal.org/ ;;;;

(in-package :aromyxo)
=common-headers=


(define-symbol-macro -> :->)


(define-symbol-macro <- :<-)


(defmacro str<-sym (sym)
  `(string-downcase (copy-seq (symbol-name ,sym))))


(defun str<-symf (sym)
  (string-downcase (copy-seq (symbol-name sym))))


(defun sym<-str (str)
  "Use `',(str->sym \"blah\") to return a quoted symbol."
  (read-from-string str))


(defmacro mksym (&rest from)
  "Use `',(mksym hello \"-world\") to return a quoted symbol."
  (let ((res ""))
    (dolist (elt from)
      (setf res
            (if (typep elt 'symbol)
                (mkstr res (str<-symf elt))
                (mkstr res elt))))
    (sym<-str res)))


(defun mksymf (&rest from)
  "Use `',(mksymf 'hello \"-world\") to return a quoted symbol."
  (let ((res ""))
    (dolist (elt from)
      (setf res
            (if (typep elt 'symbol)
                (mkstr res (str<-symf elt))
                (mkstr res elt))))
    (sym<-str res)))


(defun instance-name<- (name)
  "hello-world <- HelloWorld"
  (declare (type string name))
  (let ((res (with-output-to-string (ss)
               (map nil (lambda (char)
                          (if (upper-case-p char)
                              (progn
                                (princ #\- ss)
                                (princ (char-downcase char) ss))
                              (princ char ss)))
                    name))))
    (if (char= #\- (char res 0))
        (subseq res 1)
        res)))


(defun c-name<- (name)
 "g_initially_unowned <- GInitiallyUnowned"
 (declare (type string name))
  (nsubstitute #\_ #\- (instance-name<- name)))
