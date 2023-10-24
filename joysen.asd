;;;; joysen.asd

(asdf:defsystem #:joysen
  :description "JSON encoder library."
  :author "Frode Fjeld <frodevf@gmail.com>"
  :license  "Unlicense"
  :version "0.0.1"
  :serial t
  :depends-on (:str)
  :components ((:file "package")
               (:file "joysen")))
