;;;; http://nostdal.org/ ;;;;

(in-package :amx)


(deftype octets ()
    `(simple-array (unsigned-byte 8) (*)))
(export 'octets)
