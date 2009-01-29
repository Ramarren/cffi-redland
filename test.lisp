(in-package :redland)

(defparameter *world* nil)
(defparameter *model* nil)
(defparameter *storage* nil)
(defparameter *parser* nil)

(defparameter *null* (null-pointer))

(defcallback basic-log :int
    ((user-data :pointer) (message message-pointer))
  (declare (ignore user-data))
  (format t "Received log message code ~a level ~a from ~a: ~a"
          (%log-message-code message)
          (%log-message-level message)
          (%log-message-facility message)
          (%log-message-message message))
  1)

(defun init-test ()
  (when *world*
    (%free-world *world*))
  (when *storage*
    (%free-storage *storage*))
  (when *model*
    (%free-model *model*))
  (setf *world* (%new-world))
  (%world-set-logger *world* *null* (callback basic-log))
  (%world-open *world*)
  (setf *storage* (%new-storage *world* "hashes" "test" "hash-type='memory'"))
  (setf *model* (%new-model *world* *storage* *null*)))

(defun init-parser (&optional (kind :rdfxml))
  (when *parser*
    (%free-parser *parser*))
  (setf *parser* (ecase kind
                   (:rdfxml (%new-parser *world* "rdfxml"
                                         "application/rdf+xml"
                                         *null*))
                   (:rss (%new-parser *world* "rss-tag-soup" *null* *null*)))))

(defun princ-model ()
  (let ((stream (%model-as-stream *model*)))
   (iter (while (zerop (%stream-end stream)))
         (princ (%statement-to-string (%stream-get-object stream)))
         (terpri)
         (%stream-next stream))))