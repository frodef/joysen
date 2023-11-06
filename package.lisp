;;;; package.lisp

(defpackage #:joysen
  (:use #:cl)
  (:export
   ;; Functions
   #:encode
   #:json-object
   #:json-object*
   #:json-getter-object
   #:json-string
   #:json-array
   #:json-dict
   #:json-tuple
   #:json-integer
   #:json-decimal
   #:json-format
   #:json-yield
   #:json-optional
   #:json-assert
   #:json-bool
   #:json-choice
   #:json-choice*
   ;; Macros
   #:with-implicit-json
   ;; Variables
   #:*json-keyword-mode*
   #:*json-keyword-function*
   #:*json-quote*
   #:*json-space*
   #:*json-default-schema*
   #:*ignore-entry*))
