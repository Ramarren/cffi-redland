(defpackage #:sparql-macro
    (:use #:cl #:bpm #:iterate #:alexandria #:redland))

;;; redland dependency is there to be able to transform nodes/uris to strings

