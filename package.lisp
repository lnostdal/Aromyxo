;;;; http://nostdal.org/ ;;;;

(in-package cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :aromyxo)
    (make-package :aromyxo
                  :use (list)
                  :nicknames (list :amx))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (sym (find-package :cl))
    #| By shadowing here too, re-evaluation of this file works correctly (CL vs. the CLOSER-MOP that "won"
    previously). |#
    (shadowing-import sym (find-package :amx))
    (export sym (find-package :amx)))

  ;; Handle Common Lisp pitfall; (import 'nil) or (export 'nil) will not work!
  (shadowing-import '(cl:nil) (find-package :amx))
  (export '(nil) (find-package :amx)))

(in-package amx)


#| We solve conflicts by "brute force" here. If two packages have conflicting symbols, the package handled last will
"win". |#


(do-external-symbols (sym (find-package :closer-mop))
  (shadowing-import sym)
  (export sym))


(do-external-symbols (sym (find-package :named-readtables))
  (shadowing-import sym)
  (export sym))


(do-external-symbols (sym (find-package :cl-utilities))
  (shadowing-import sym)
  (export sym))


(do-external-symbols (sym (find-package :alexandria))
  (shadowing-import sym)
  (export sym))


(do-external-symbols (sym (find-package :sb-thread))
  (shadowing-import sym)
  (export sym))


(do-external-symbols (sym (find-package :bordeaux-threads))
  (shadowing-import sym)
  (export sym))
