;;;; http://nostdal.org/ ;;;;

(defpackage #:am-web
  (:use #:cl #:am-util #:alexandria)
  (:nicknames #:amw))

(in-package #:amw)


(export '(url-encode url-decode
          rfc-1123-date iso-time
          escape-for-html))
