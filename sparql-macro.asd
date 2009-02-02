(asdf:defsystem sparql-macro
  :version "0"
  :description "Macro syntax for SPARQL RDF query language"
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:bpm :redland :iterate :alexandria)
  :components ((:file "sparql-macro")))
