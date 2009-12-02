;;;; http://nostdal.org/ ;;;;

(in-package :am-util)


;;; Functions for manipulating alists.

(defun aget (alist key &key (test #'string-equal) return-if-not-found)
  "Returns the association with given `key' or nil if none matches.
Settable."
  (assoc key alist :test test))
