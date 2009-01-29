(in-package :redland)

(define-foreign-library redland
  (t (:default "librdf")))

(define-foreign-library rasqal
  (t (:default "librasqal")))

(define-foreign-library raptor
  (t (:default "libraptor")))

(defun load-redland-libraries ()
  (use-foreign-library redland)
  (use-foreign-library rasqal)
  (use-foreign-library raptor))
