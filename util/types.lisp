;;;; http://nostdal.org/ ;;;;

(in-package :amx)


;; A vector of octets that is not displaced, has no fill pointer and is not adjustable.
(deftype octets ()
    `(simple-array (unsigned-byte 8) (*)))
(export 'octets)
