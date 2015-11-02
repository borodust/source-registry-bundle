(defpackage :source-registry-bundle-def
  (:use :cl :asdf))


(in-package :source-registry-bundle-def)


(defsystem :source-registry-bundle
  :author "Pavel Korolev <dev@borodust.org>"
  :description "Packaging operation for asdf"
  :version "0.9.0"
  :depends-on (asdf alexandria cl-fad)
  :components ((:file "source-registry-bundle")))
