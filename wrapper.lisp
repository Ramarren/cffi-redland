(in-package :redland)

;;; TODO: add error signalling on failures

;;; Not sure this is a good idea, but manual memory management does seem really annoying, as many
;;; objects seem to have not easily delimited lifetime

(defparameter *null* (null-pointer))
(defvar *world-life* 0)


(defclass pointer-wrapper ()
  ((pointer :initform (make-array 3 :initial-contents (list nil *null* *world-life*))
            :initarg :pointer-array
            :accessor pointer-array)
   (type :initform nil
         :initarg :type
         :accessor get-type)))

(defun free-object (pointer type)
  (ecase type
    (world
       (gc :full t)
       (incf *world-life*)
       (%free-world pointer))
    (node (%free-node pointer))
    (statement (%free-statement pointer))
    (uri (%free-uri pointer))
    (model (%free-model pointer))
    (storage (%free-storage pointer))
    (parser (%free-parser pointer))
    (iterator (%free-iterator pointer))
    (node-iterator (%free-iterator pointer))
    (statement-iterator (%free-iterator pointer))
    (redland-list (%free-list pointer))
    (hash (%free-hash pointer))
    (redland-stream (%free-stream pointer))
    (node-stream (%free-stream pointer))
    (statement-stream (%free-stream pointer))
    (query (%free-query pointer))
    (query-results (%free-query-results pointer))))

(defun maybe-free-pointer-array (arry type)
  (when (aref arry 0)
    (setf (aref arry 0) nil)
    (free-object (aref arry 1) type)
    (setf (aref arry 1) *null*)
    (values t)))

(defun unown-pointer (wrapper)
  (cancel-finalization wrapper)
  (setf (aref (pointer-array wrapper) 0) nil))

(declaim (inline wrap-pointer wrap-shared-pointer))

(defun wrap-shared-pointer (pointer type)
  (make-instance type
                 :pointer-array (make-array 3 :initial-contents (list nil pointer *world-life*))
                 :type type))

(defun wrap-pointer (pointer type)
  (unless (null-pointer-p pointer)
   (let ((wrapper (wrap-shared-pointer pointer type)))
     (setf (aref (pointer-array wrapper) 0) t)
     (finalize wrapper (curry #'maybe-free-pointer-array (pointer-array wrapper) type))
     wrapper)))

(declaim (inline get-pointer))
(defgeneric get-pointer (wrapper)
  (:method ((wrapper t))
    wrapper)
  (:method ((wrapper pointer-wrapper))
    (let ((arry (pointer-array wrapper)))
      (aref arry 1))))

(defgeneric free-pointer (wrapper)
  (:method ((wrapper pointer-wrapper))
    (prog1
        (maybe-free-pointer-array (pointer-array wrapper) (get-type wrapper))
      (cancel-finalization wrapper))))

;;; classes

(defclass world (pointer-wrapper)
  ())

(defclass storage (pointer-wrapper)
  ())

(defclass model (pointer-wrapper)
  ())

(defclass node (pointer-wrapper)
  ())

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity nil)
    (princ (%node-to-string (get-pointer node)) stream)))

(defclass statement (pointer-wrapper)
  ())

(defmethod print-object ((statement statement) stream)
  (print-unreadable-object (statement stream :type t :identity nil)
    (princ (%statement-to-string (get-pointer statement)) stream)))

(defclass uri (pointer-wrapper)
  ())

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity nil)
    (princ (%uri-to-string (get-pointer uri)) stream)))

(defclass parser (pointer-wrapper)
  ())

(defclass serializer (pointer-wrapper)
  ())

(defclass digest (pointer-wrapper)
  ())

(defclass hash (pointer-wrapper)
  ())

(defclass iterator (pointer-wrapper)
  ())

(defclass node-iterator (iterator)
  ())

(defclass statement-iterator (iterator)
  ())

(defclass redland-list (pointer-wrapper)
  ())

(defclass redland-stream (pointer-wrapper)
  ())

(defclass node-stream (redland-stream)
  ())

(defclass statement-stream (redland-stream)
  ())

(defclass query (pointer-wrapper)
  ())

(defclass query-results (pointer-wrapper)
  ())

;;; special variables
(defvar *world* nil)
(defvar *storage* nil)
(defvar *model* nil)
(defvar *parser* nil)

;;; world
;;; constructor and with- macros

(defun make-world ()
  (wrap-pointer (%new-world) 'world))

(defun world-open (&optional (world *world*))
  (%world-open (get-pointer world)))

(defmacro with-world (&body body)
  `(let ((*world* (make-world)))
     (unwind-protect
          (progn ,@body)
       (free-pointer *world*))))

;;; TODO add lispified logging 
;;; TODO ignore digest for now
;;; TODO ignore features (what are those anyway?)

;;; concepts

(defun get-concept-resource (concept &optional (world *world*))
  (wrap-shared-pointer (%get-concept-resource-by-index (get-pointer world) concept) 'node))

(defun get-concept-uri (concept &optional (world *world*))
  (wrap-shared-pointer (%get-concept-uri-by-index (get-pointer world) concept) 'uri))

;;; TODO ignore digests for now

;;; TODO hashes, figure out how often are those used directly

;;; Heuristics

(defun gen-name (base-name)
  (%heuristic-gen-name base-name))

(defun is-blank-node (node-name)
  (%heuristic-is-blank-node node-name))

(defun get-blank-node (node-string)
  (%heuristic-get-blank-node node-string))

(defun object-is-literal (object-string)
  (not (zerop (%heuristic-object-is-literal object-string))))

;;; Iterator

;;; which parts are relevant for common use?
;;; certainly get-object, next and end

(defun iterator-next (iterator)
  (zerop (%iterator-next (get-pointer iterator))))

(defun iterator-endp (iterator)
  (not (zerop (%iterator-end (get-pointer iterator)))))

(defgeneric iterator-get-object (iterator)
  (:method ((iterator iterator))
    (%iterator-get-object (get-pointer iterator)))
  (:method ((iterator node-iterator))
    (wrap-pointer (call-next-method) 'node))
  (:method ((iterator statement-iterator))
    (wrap-pointer (call-next-method) 'statement)))

;;; TODO List
;;; I don't think they even come from anywhere?

;;; TODO Logging
;;; as above, message decomposition suppoer

;;; Model
;;; dropping less comprehensible functions

(defun make-model (&key (world *world*) (storage *storage*) (options *null*))
  (wrap-pointer (%new-model (get-pointer world) (get-pointer storage) options) 'model))

(defmacro with-model ((&key (world *world*) (storage *storage*) (options *null*)) &body body)
  `(let ((*model* (make-model :world ,world :storage ,storage :options ,options)))
     (unwind-protect (progn ,@body)
       (free-pointer *model*))))

(defun model-size (&optional (model *model*))
  (%model-size (get-pointer model)))

(defun model-add (subject predicate object &optional (model *model*))
  (unown-pointer subject)
  (unown-pointer predicate)
  (unown-pointer object)
  (zerop (%model-add (get-pointer model) (get-pointer subject)
                     (get-pointer predicate) (get-pointer object))))

(defun model-add-string-literal-statement (subject predicate literal
                                           &key (language *null*) (model *model*) (is-wf-xml nil))
  (unown-pointer subject)
  (unown-pointer predicate)
  (zerop (%model-add-string-literal-statement (get-pointer model)
                                              (get-pointer subject)
                                              (get-pointer predicate)
                                              literal
                                              language
                                              (if is-wf-xml 1 0))))

(defun model-add-typed-literal-statement (subject predicate literal
                                          &key (model *model*) (language *null*) (datatype-uri *null*))
  (unown-pointer subject)
  (unown-pointer predicate)
  (zerop (%model-add-typed-literal-statement (get-pointer model) (get-pointer subject)
                                             (get-pointer predicate) literal language datatype-uri)))

(defun model-add-statement (statement &optional (model *model*))
  (zerop (%model-add-statement (get-pointer model) (get-pointer statement))))

(defun model-add-statements (stream &optional (model *model*))
  (zerop (%model-add-statements (get-pointer model) (get-pointer stream))))

(defun model-remove-statement (statement &optional (model *model*))
  (zerop (%model-remove-statement (get-pointer model) (get-pointer statement))))

(defun model-contains-statement-p (statement &optional (model *model*))
  (not (zerop (%model-contains-statement (get-pointer model) (get-pointer statement)))))

(defun model-has-arc-in (node property &optional (model *model*))
  (not (zerop (%model-has-arc-in (get-pointer model) (get-pointer node) (get-pointer property)))))

(defun model-has-arc-out (node property &optional (model *model*))
  (not (zerop (%model-has-arc-out (get-pointer model) (get-pointer node) (get-pointer property)))))

(defun model-as-stream (&optional (model *model*))
  (wrap-pointer (%model-as-stream (get-pointer model)) 'statement-stream))

(defun model-find-statements (statement &optional (model *model*))
  (wrap-pointer (%model-find-statements (get-pointer model) (get-pointer statement)) 'statement-stream))

;;; TODO: find with options, requires hashes above

(defmacro define-get-function (what first second)
  `(defun ,(symbolicate 'model-get- what) (,first ,second &optional (model *model*))
     (wrap-pointer (,(symbolicate '%model-get- what) (get-pointer model)
                     (get-pointer ,first) (get-pointer ,second)) 'node-iterator)))

(define-get-function sources arc target)
(define-get-function arcs source target)
(define-get-function targets source arc)

(defmacro define-single-get-function (what first second)
  `(defun ,(symbolicate 'model-get- what) (,first ,second &optional (model *model*))
     (wrap-pointer (,(symbolicate '%model-get- what) (get-pointer model)
                     (get-pointer ,first) (get-pointer ,second)) 'node)))

(define-single-get-function source arc target)
(define-single-get-function arc source target)
(define-single-get-function target source arc)

(defun model-get-arcs-in (node &optional (model *model*))
  (wrap-pointer (%model-get-arcs-in (get-pointer model) (get-pointer node)) 'node-iterator))

(defun model-get-arcs-out (node &optional (model *model*))
  (wrap-pointer (%model-get-arcs-out (get-pointer model) (get-pointer node)) 'node-iterator))

;;; submodels?
;;; print? how to handle FILE*, anyway?

(defun model-context-add-statement (context statement &optional (model *model*))
  (zerop (%model-context-add-statement (get-pointer model) (get-pointer context) (get-pointer statement))))

(defun model-context-add-statements (context statements &optional (model *model*))
  (zerop (%model-context-add-statements (get-pointer model) (get-pointer context) (get-pointer statements))))

(defun model-context-remove-statement (context statement &optional (model *model*))
  (zerop (%model-context-remove-statement (get-pointer model) (get-pointer context) (get-pointer statement))))

(defun model-context-remove-statements (context &optional (model *model*))
  (zerop (%model-context-remove-statements (get-pointer model) (get-pointer context))))

(defun model-context-as-stream (context &optional (model *model*))
  (wrap-pointer (%model-context-as-stream (get-pointer model) (get-pointer context)) 'statement-stream))

(defun model-contains-context (context &optional (model *model*))
  (not (zerop (%model-contains-context (get-pointer model) (get-pointer context)))))

(defun model-query-execute (query &optional (model *model*))
  (wrap-pointer (%model-query-execute (get-pointer model) (get-pointer query)) 'query-results))

(defun model-sync (&optional (model *model*))
  (%model-sync (get-pointer model)))

(defun model-get-storage (&optional (model *model*))
  (wrap-pointer (%model-get-storage (get-pointer model)) 'storage))

(defun model-load (uri &key (model *model*) (name *null*) (mime-type *null*) (type-uri *null*))
  (zerop (%model-load (get-pointer model) (get-pointer uri) name mime-type (get-pointer type-uri))))

;;; TODO: model to counted string

(defun model-to-string (&key (model *model*) (base-uri *null*)
                        (name *null*) (mime-type *null*) (type-uri *null*))
  (%model-to-string (get-pointer model) (get-pointer base-uri) name
                    mime-type (get-pointer type-uri)))

(defun model-find-statements-in-context (statement context &optional (model *model*))
  (wrap-pointer (%model-find-statements-in-context (get-pointer model)
                                                   (get-pointer statement)
                                                   (get-pointer context))
                'statement-stream))

(defun model-get-contexts (&optional (model *model*))
  (wrap-pointer (%model-get-contexts (get-pointer model)) 'node-iterator))

;;; TODO: model features

(defun model-transaction-commit (&optional (model *model*))
  (zerop (%model-transaction-commit (get-pointer model))))

(defun model-transaction-rollback (&optional (model *model*))
  (zerop (%model-transaction-rollback (get-pointer model))))

(defun model-transaction-start (&optional (model *model*))
  (zerop (%model-transaction-start (get-pointer model))))

;;; handles, what are those?

;;; Nodes

(defun make-private-node (&optional (world *world*))
  (wrap-pointer (%new-node (get-pointer world)) 'node))

(defun make-node-from-uri-string (uri-string &optional (world *world*))
  (wrap-pointer (%new-node-from-uri-string (get-pointer world) uri-string) 'node))

(defun make-node-from-uri (uri &optional (world *world*))
  (wrap-pointer (%new-node-from-uri (get-pointer world) (get-pointer uri)) 'node))

(defun make-node-from-uri-local-name (uri local-name &optional (world *world*))
  (wrap-pointer (%new-node-from-uri-local-name (get-pointer world)
                                               (get-pointer uri)
                                               local-name)
                'node))

(defun make-node-from-normalised-uri-string (uri-string source-uri base-uri &optional (world *world*))
  (wrap-pointer (%new-node-from-normalised-uri-string (get-pointer world) uri-string source-uri base-uri)
                'node))

(defun make-node-from-literal (string &key (world *world*) (language *null*) (is-wf-xml nil))
  (wrap-pointer (%new-node-from-literal (get-pointer world) string language (if is-wf-xml 1 0)) 'node))

(defun make-node-from-typed-literal (string &key (world *world*) (language *null*) (datatype-uri *null*))
  (wrap-pointer (%new-node-from-typed-literal (get-pointer world)
                                              string
                                              language (get-pointer datatype-uri)) 'node))

(defun make-node-from-blank-identifier (identifier &optional (world *world*))
  (wrap-pointer (%new-node-from-blank-identifier (get-pointer world) identifier) 'node))

(defun copy-node (node)
  (wrap-pointer (%new-node-from-node (get-pointer node)) 'node))

(defun node-get-uri (node)
  (wrap-shared-pointer (%node-get-uri (get-pointer node)) 'uri))

(defun node-get-type (node)
  (%node-get-type (get-pointer node)))

(defun node-get-literal-value (node)
  (%node-get-literal-value (get-pointer node)))

;;; TODO: some more node retrievals

(defun node-get-li-ordinal (node)
  (%node-get-li-ordinal (get-pointer node)))

(defun node-get-blank-identifier (node)
  (%node-get-blank-identifier (get-pointer node)))

(defun node-is-resource-p (node)
  (not (zerop (%node-is-resource (get-pointer node)))))

(defun node-is-literal-p (node)
  (not (zerop (%node-is-literal (get-pointer node)))))

(defun node-is-blank-p (node)
  (not (zerop (%node-is-blank (get-pointer node)))))

(defun node-to-string (node)
  (%node-to-string (get-pointer node)))

(defun node-equals-p (node1 node2)
  (zerop (%node-equals (get-pointer node1) (get-pointer node2))))

;;; Parsers

;;; skipping some function

(defun make-parser (name &key (world *world*) (mime-type *null*) (type-uri *null*))
  (wrap-pointer (%new-parser (get-pointer world) name mime-type (get-pointer type-uri)) 'parser))

(defun parse-as-stream (parser uri &optional (base-uri *null*))
  (wrap-pointer (%parser-parse-as-stream (get-pointer parser) (get-pointer uri)
                                         (get-pointer base-uri))
                'statement-stream))

(defun parse-into-model (parser uri &key (model *model*) (base-uri *null*))
  (zerop (%parser-parse-into-model (get-pointer parser)
                                   (get-pointer uri)
                                   (get-pointer base-uri)
                                   (get-pointer model))))

(defun parse-string-as-stream (parser string &optional (base-uri *null*))
  (wrap-pointer (%parser-parse-as-stream (get-pointer parser) string
                                         (get-pointer base-uri))
                'statement-stream))

(defun parse-string-into-model (parser string &key (model *model*) (base-uri *null*))
  (zerop (%parser-parse-into-model (get-pointer parser)
                                   string
                                   (get-pointer base-uri)
                                   (get-pointer model))))

;;; skipping namespaces

;;; TODO: Query/Query result/Serialization

;;; Statement

(defun make-statement (&optional (world *world*))
  (wrap-pointer (%new-statement (get-pointer world)) 'statement))

(defun copy-statement (statement)
  (wrap-pointer (%new-statement-from-statement (get-pointer statement)) 'statement))

(defun make-statement-from-nodes (subject predicate object &optional (world *world*))
  (unown-pointer subject)
  (unown-pointer predicate)
  (unown-pointer object)
  (wrap-pointer (%new-statement-from-nodes (get-pointer world)
                                           (get-pointer subject)
                                           (get-pointer predicate)
                                           (get-pointer object)) 'statement))

(defun statement-clear (statement)
  (%statement-clear (get-pointer statement)))

(defmacro statement-get-set (&rest what)
  (cons 'progn
        (iter (for w in what)
              (appending
               (list `(defun ,(symbolicate 'statement-get- w) (statement)
                        (wrap-shared-pointer (,(symbolicate '%statement-get- w) (get-pointer statement))
                                             'node))
                     `(defun ,(symbolicate 'statement-set- w) (statement ,w)
                        (unown-pointer ,w)
                        (,(symbolicate '%statement-set- w) (get-pointer statement) (get-pointer ,w))))))))

(statement-get-set subject predicate object)

(defun statement-is-complete-p (statement)
  (not (zerop (%statement-is-complete (get-pointer statement)))))

(defun statement-to-string (statement)
  (%statement-to-string (get-pointer statement)))

(defun statement-equals-p (statement1 statement2)
  (not (zerop (%statement-equals (get-pointer statement1)
                                 (get-pointer statement2)))))

(defun statement-match-p (statement partial-statement)
  (not (zerop (%statement-match (get-pointer statement)
                                (get-pointer partial-statement)))))

;;; TODO: encode/decode

;;; Triple stores

;;; skipping implementation methods

(defun storage-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer))
    (let ((retval (%storage-enumerate (get-pointer world) counter name label)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer)))))))

(defun make-storage (storage-name name options &optional (world *world*))
  (wrap-pointer (%new-storage (get-pointer world)
                              storage-name name options) 'storage))

(defmacro with-storage ((storage-name name options &optional (world *world*)) &body body)
  `(let ((*storage* (make-storage ,storage-name ,name ,options ,world)))
     (unwind-protect (progn ,@body)
       (free-pointer *storage*))))

;;; TODO: storage with options, requires hashes

(defun copy-storage (old-storage)
  (wrap-pointer (%new-storage-from-storage (get-pointer old-storage)) 'storage))

;;; Streams

;;; like with iterators, minimal interface

(defun stream-next (stream)
  (zerop (%stream-next (get-pointer stream))))

(defun stream-endp (stream)
  (not (zerop (%stream-end (get-pointer stream)))))

(defgeneric stream-get-object (stream)
  (:method ((stream redland-stream))
    (%stream-get-object (get-pointer stream)))
  (:method ((stream node-stream))
    (wrap-pointer (call-next-method) 'node))
  (:method ((stream statement-stream))
    (wrap-pointer (call-next-method) 'statement)))

;;; URI

(defun make-uri (uri-string &optional (world *world*))
  (wrap-pointer (%new-uri (get-pointer world) uri-string) 'uri))

(defun copy-uri (old-uri)
  (wrap-pointer (%new-uri-from-uri (get-pointer old-uri)) 'uri))

(defun make-uri-from-uri-local-name (old-uri local-name)
  (wrap-pointer (%new-uri-from-uri-local-name (get-pointer old-uri) local-name) 'uri))

(defun uri-as-string (uri)
  (%uri-as-string (get-pointer uri)))

(defun uri-to-string (uri)
  (%uri-to-string (get-pointer uri)))

(defun uri-equals (uri1 uri2)
  (not (zerop (%uri-equals (get-pointer uri1)
                           (get-pointer uri2)))))

(defun is-file-uri-p (uri)
  (not (zerop (%uri-is-file-uri (get-pointer uri)))))

(defun uri-to-filename (uri)
  (%uri-to-filename (get-pointer uri)))

(defun make-uri-normalised-to-base (uri-string source-uri base-uri)
  (wrap-pointer (%new-uri-normalised-to-base uri-string
                                             (get-pointer source-uri)
                                             (get-pointer base-uri))
                'uri))

(defun make-uri-relative-to-base (base-uri uri-string)
  (wrap-pointer (%new-uri-relative-to-base (get-pointer base-uri)
                                           uri-string)
                'uri))

(defun make-uri-from-filename (filename &optional (world *world*))
  (wrap-pointer (%new-uri-from-filename (get-pointer world) (namestring filename)) 'uri))

(defun uri-compare (uri1 uri2)
  (%uri-compare (get-pointer uri1) (get-pointer uri2)))