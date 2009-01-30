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

(defun test-mid-level-query ()
  (with-world (:log-function (make-log-everything *standard-output*))
    (with-storage ("hashes" "test" "hash-type='memory'")
      (with-model ()
        (model-load (make-uri "http://ramarren.blox.pl/rss2"))
        (let ((query (make-query "PREFIX rss: <http://purl.org/rss/1.0/>
                                  SELECT ?title ?y
                                  WHERE {
                                   ?x rss:items ?items .
                                   ?items ?arc ?y .
                                   ?y rss:title ?title}")))
          (iter (for alist in-query-results (query-execute query))
                (print alist)))))))

(defun test-query-numop ()
  (with-world (:log-function (make-log-everything *standard-output*))
    (with-storage ("hashes" "test" "hash-type='memory'")
      (with-model ()
        (model-add-statement (make-statement-from-nodes
                              (make-node-from-uri-string "http://ramarren.blox.pl")
                              (make-node-from-uri-string "tag:visits-number")
                              (make-node-from-typed-literal "42"
                                                            :datatype-uri
                                                            (make-uri "http://www.w3.org/2001/XMLSchema#integer"))))
        (let ((query (make-query "PREFIX rss: <http://purl.org/rss/1.0/>
                                  SELECT ?what ?num
                                  WHERE {
                                   ?what <tag:visits-number> ?num .
                                   FILTER (?num > 23)}")))
          (assert query)
          (iter (for alist in-query-results (query-execute query))
                (print alist)))))))