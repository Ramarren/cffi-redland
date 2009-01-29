(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem redland
  :version "0"
  :description "CFFI bindings for redland RDF library."
  :maintainer "Jakub Higersberger <ramarren@gmail.com>"
  :author "Jakub Higersberger <ramarren@gmail.com>"
  :licence "BSD-style"
  :depends-on (:cffi :alexandria :iterate)
  :components ((:file "package")
               (:file "libraries" :depends-on ("package"))
               (cffi-grovel:grovel-file "redland-grovel" :depends-on ("package"))
               (:file "bindings" :depends-on ("package" "redland-grovel" "libraries"))
               (:file "wrapper" :depends-on ("package" "bindings"))))
