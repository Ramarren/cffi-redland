(in-package :redland)

;;; TODO: add error signalling on failures

;;; Not sure this is a good idea, but manual memory management does seem really annoying, as many
;;; objects seem to have not easily delimited lifetime

(defparameter *null* (null-pointer))

;;; conditions

(define-condition redland-error (simple-error)
  ())

(define-condition redland-construction-error (redland-error)
  ())

(defun signal-construction-error (what)
  (error 'redland-construction-error
         :format-control "Failed to construct ~a"
         :format-arguments (list what)))

(define-condition redland-feature-error (redland-error)
  ((exists :reader exists :initarg :exists)))

(defun signal-feature-error (what feature exists)
  (error 'redland-feature-error
         :exists exists
         :format-control (if exists
                             "Failed to set feature ~a of ~a"
                             "There is no feature ~a in ~a")
         :format-arguments (list feature what)))

(define-condition redland-statement-add-error (redland-error)
  ())

(define-condition redland-transaction-error (redland-error)
  ())

(define-condition redland-type-error (redland-error)
  ((is :initarg :is :reader is-of)
   (should-be :initarg :should-be :reader should-be-of))
  (:report (lambda (c stream)
             (format stream "Wanted pointer-wrapper of type ~a, tried to unwrap ~a"
                     (should-be-of c) (is-of c)))))

;; pointer-array: #(is-owned-by-lisp pointer world-object)
(defclass pointer-wrapper ()
  ((pointer :initform (make-array 3 :initial-contents (list nil *null* nil))
            :initarg :pointer-array
            :accessor pointer-array)
   (type :initform nil
         :initarg :type
         :accessor get-type)))

(declaim (inline get-world))
(defun get-world (wrapper)
  (aref (pointer-array wrapper) 2))

(defun free-object (pointer type)
  (ecase type
    (world
       (%free-world pointer))
    (node (%free-node pointer))
    (statement (%free-statement pointer))
    (uri (%free-uri pointer))
    (model (%free-model pointer))
    (storage (%free-storage pointer))
    (parser (%free-parser pointer))
    (serializer (%free-serializer pointer))
    (iterator (%free-iterator pointer))
    (node-iterator (%free-iterator pointer))
    (redland-list (%free-list pointer))
    (hash (%free-hash pointer))
    (redland-stream (%free-stream pointer))
    (statement-stream (%free-stream pointer))
    (query (%free-query pointer))
    (query-results (%free-query-results pointer))
    (query-results-formatter (%free-query-results-formatter pointer))))

(defun maybe-free-pointer-array (arry type)
  (when (and (aref arry 0);owned by Lisp
             (or (eql type 'world)
                 (world-alive-p (aref arry 2)))) ;object world hasn't been freed already
    (setf (aref arry 0) nil)
    (free-object (aref arry 1) type)
    (setf (aref arry 1) *null*)
    (values t)))

(defun unown-pointer (wrapper)
  (cancel-finalization wrapper)
  (setf (aref (pointer-array wrapper) 0) nil))

(declaim (inline wrap-pointer wrap-shared-pointer))

(defun wrap-shared-pointer (pointer type world)
  (make-instance type
                 :pointer-array (make-array 3 :initial-contents (list nil pointer world))
                 :type type))

(defun wrap-pointer (pointer type world)
  (unless (null-pointer-p pointer)
   (let ((wrapper (wrap-shared-pointer pointer type world)))
     (setf (aref (pointer-array wrapper) 0) t)
     (finalize wrapper (curry #'maybe-free-pointer-array (pointer-array wrapper) type))
     wrapper)))

(declaim (inline get-pointer))
(defgeneric get-pointer (wrapper type)
  (:method ((wrapper t) type)
    (declare (ignore type))
    wrapper)
  (:method ((wrapper pointer-wrapper) type)
    (unless (or (null type)
                (eql (get-type wrapper) type))
      (error 'redland-type-error :is (get-type wrapper) :should-be type))
    (let ((arry (pointer-array wrapper)))
      (aref arry 1))))

(defgeneric free-pointer (wrapper)
  (:method :before ((wrapper world))
    (setf (world-alive-p wrapper) nil))
  (:method ((wrapper pointer-wrapper))
    (prog1
        (maybe-free-pointer-array (pointer-array wrapper) (get-type wrapper))
      (cancel-finalization wrapper))))

;;; classes

(defclass world (pointer-wrapper)
  ((world-alive :initform t :accessor world-alive-p)))

(defclass storage (pointer-wrapper)
  ())

(defclass model (pointer-wrapper)
  ())

(defclass node (pointer-wrapper)
  ())

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t :identity nil)
    (if (world-alive-p (get-world node))
        (princ (%node-to-string (get-pointer node 'node)) stream)
        (princ 'invalidated stream))))

(defclass statement (pointer-wrapper)
  ())

(defmethod print-object ((statement statement) stream)
  (print-unreadable-object (statement stream :type t :identity nil)
    (if (world-alive-p (get-world statement))
        (princ (%statement-to-string (get-pointer statement 'statement)) stream)
        (princ 'invalid stream))))

(defclass uri (pointer-wrapper)
  ())

(defmethod print-object ((uri uri) stream)
  (print-unreadable-object (uri stream :type t :identity nil)
    (if (world-alive-p (get-world uri))
        (princ (%uri-to-string (get-pointer uri 'uri)) stream)
        (princ 'invalid stream))))

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

(defclass redland-list (pointer-wrapper)
  ())

(defclass redland-stream (pointer-wrapper)
  ())

(defclass statement-stream (redland-stream)
  ())

(defclass query (pointer-wrapper)
  ())

(defclass query-results (pointer-wrapper)
  ())

(defclass query-results-formatter (pointer-wrapper)
  ())

;;; special variables
(defvar *world* nil)
(defvar *storage* nil)
(defvar *model* nil)
(defvar *parser* nil)

;;; world
;;; constructor and with- macros

(defun make-world ()
  (let ((new-world (%new-world)))
    (if (null-pointer-p new-world)
        (signal-construction-error 'world)
        (wrap-pointer new-world 'world nil))))

(defun world-open (&optional (world *world*))
  (%world-open (get-pointer world 'world)))

(defmacro with-world ((&key (log-function '*log-function*)) &body body)
  `(let ((*world* (make-world))
         (*world-life* (1- (incf *world-life*))))
     (with-log-function (,log-function)
       (unwind-protect
            (progn ,@body)
         (free-pointer *world*)))))

;;; logging support, with dynamic scope... this doesn't allow multiple worlds loggers, but would it?

(defvar *log-function* (constantly nil))

(defcallback log-callback :int
    ((user-data :pointer) (message message-pointer))
  (declare (ignore user-data))
  (if (funcall *log-function*
               :code (%log-message-code message)
               :level (%log-message-level message)
               :from (%log-message-facility message)
               :message (%log-message-message message))
      1 0))

(defun make-log-everything (stream)
  #'(lambda (&key code level from message)
      (format stream "Received log message code ~a level ~a from ~a: ~a"
              code level from message)))

(defun set-log-function (log-function &optional (world *world*))
  (assert world)
  (setf *log-function* log-function)
  (%world-set-logger (get-pointer world 'world) *null* (callback log-callback))
  (values))

(defmacro with-log-function ((log-function &optional (world '*world*)) &body body)
  `(let ((*log-function* ,log-function))
     (%world-set-logger (get-pointer ,world 'world) *null* (callback log-callback))
     ,@body))

(defmacro with-logging ((stream &optional (world '*world*)) &body body)
  `(let ((*log-function* (make-log-everything ,stream)))
     (%world-set-logger (get-pointer ,world 'world) *null* (callback log-callback))
     ,@body))

(defun world-set-digest (name &optional (world *world*))
  (%world-set-digest (get-pointer world 'world) name)
  (values))

(defun world-get-feature (world feature)
  (wrap-pointer (%world-get-feature (get-pointer world 'world) (get-pointer feature 'node)) 'node world))

(defun world-set-feature (world feature value)
  (let ((result (%world-set-feature (get-pointer world 'world) (get-pointer feature 'node)
                                    (get-pointer value 'node))))
    (cond ((plusp result) (signal-feature-error world feature t))
          ((zerop result) t)
          ((minusp result) ))))

;;; concepts

(defun get-concept-resource (concept &optional (world *world*))
  (let ((resource (%get-concept-resource-by-index (get-pointer world 'world) concept)))
    (if resource
        (wrap-shared-pointer resource 'node world)
        (error 'redland-error :format-control "Can't find concept resource for ~a"
            :format-arguments (list concept)))))

(defun get-concept-uri (concept &optional (world *world*))
  (let ((uri (%get-concept-uri-by-index (get-pointer world 'world) concept)))
    (if uri
        (wrap-shared-pointer uri 'uri world)
        (error 'redland-error
               :format-control "Can't find concept uri for ~a"
               :format-arguments (list uri)))))

;;; TODO ignore digests for now

;;; TODO hashes, figure out how often are those used directly

;;; Heuristics

(defun gen-name (base-name)
  (let ((new-name (%heuristic-gen-name base-name)))
    (if new-name new-name
        (error 'redland-error :format-control "Failed to make a new name from ~a"
               :format-arguments (list base-name)))))

(defun is-blank-node (node-name)
  (not (zerop (%heuristic-is-blank-node node-name))))

(defun get-blank-node (node-string)
  (%heuristic-get-blank-node node-string))

(defun object-is-literal (object-string)
  (not (zerop (%heuristic-object-is-literal object-string))))

;;; Iterator

;;; which parts are relevant for common use?
;;; certainly get-object, next and end

(defun iterator-next (iterator)
  (zerop (%iterator-next (get-pointer iterator 'node-iterator))))

(defun iterator-endp (iterator)
  (not (zerop (%iterator-end (get-pointer iterator 'node-iterator)))))

(defun iterator-get-object (iterator)
  (wrap-shared-pointer (%iterator-get-object (get-pointer iterator 'iterator))
                       'node (get-world iterator)))

;;; TODO List
;;; I don't think they even come from anywhere?

;;; Model
;;; dropping less comprehensible functions

(defun model-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer))
    (let ((retval (%model-enumerate (get-pointer world 'world) counter name label)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer)))))))

(defun make-model (&key (world *world*) (storage *storage*) (options *null*))
  (let ((new-model (%new-model (get-pointer world 'world) (get-pointer storage 'storage) options)))
    (if (null-pointer-p new-model)
        (signal-construction-error 'model)
        (wrap-pointer new-model 'model world))))

(defmacro with-model ((&key (world '*world*) (storage '*storage*) (options '*null*)) &body body)
  `(let ((*model* (make-model :world ,world :storage ,storage :options ,options)))
     (unwind-protect (progn ,@body)
       (free-pointer *model*))))

(defun model-size (&optional (model *model*))
  (let ((size (%model-size (get-pointer model 'model))))
    (unless (minusp size)
      size)))

(defun model-add (subject predicate object &optional (model *model*))
  (unown-pointer subject)
  (unown-pointer predicate)
  (unown-pointer object)
  (let ((ret-code (%model-add (get-pointer model 'model) (get-pointer subject 'node)
                              (get-pointer predicate 'node) (get-pointer object 'node))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-add-string-literal-statement (subject predicate literal
                                           &key (language *null*) (model *model*) (is-wf-xml nil))
  (unown-pointer subject)
  (unown-pointer predicate)
  (let ((ret-code (%model-add-string-literal-statement (get-pointer model 'model)
                                                       (get-pointer subject 'node)
                                                       (get-pointer predicate 'node)
                                                       literal
                                                       language
                                                       (if is-wf-xml 1 0))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-add-typed-literal-statement (subject predicate literal
                                          &key (model *model*) (language *null*) (datatype-uri *null*))
  (unown-pointer subject)
  (unown-pointer predicate)
  (let ((ret-code (%model-add-typed-literal-statement (get-pointer model 'model) (get-pointer subject 'node)
                                                      (get-pointer predicate 'node) literal language datatype-uri)))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-add-statement (statement &optional (model *model*))
  (let ((ret-code (%model-add-statement (get-pointer model 'model) (get-pointer statement 'statement))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-add-statements (stream &optional (model *model*))
  (let ((ret-code (%model-add-statements (get-pointer model 'model) (get-pointer stream 'statement-stream))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-remove-statement (statement &optional (model *model*))
  (let ((ret-code (%model-remove-statement (get-pointer model 'model) (get-pointer statement 'statement))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-contains-statement-p (statement &optional (model *model*))
  (let ((ret-val (%model-contains-statement (get-pointer model 'model) (get-pointer statement 'statement))))
    (cond ((> ret-val 0) (error 'redland-error :format-control "Invalid statement passed to model-contains-statement-p"))
          ((zerop ret-val) nil)
          ((< ret-val 0) t))))

(defun model-has-arc-in (node property &optional (model *model*))
  (not (zerop (%model-has-arc-in (get-pointer model 'model) (get-pointer node 'node) (get-pointer property 'node)))))

(defun model-has-arc-out (node property &optional (model *model*))
  (not (zerop (%model-has-arc-out (get-pointer model 'model) (get-pointer node 'node) (get-pointer property 'node)))))

(defun model-as-stream (&optional (model *model*))
  (wrap-pointer (%model-as-stream (get-pointer model 'model)) 'statement-stream (get-world model)))

(defun model-find-statements (statement &optional (model *model*))
  (let ((stream (%model-find-statements (get-pointer model 'model) (get-pointer statement 'statement))))
    (if (null-pointer-p stream)
        (error 'redland-error :format-control "Error in find-statements.")
        (wrap-pointer stream 'statement-stream (get-world model)))))

(defun model-find-statements-with-nodes (&key (subject *null*) (predicate *null*) (object *null*) (world *world*) (model *model*))
  (model-find-statements (make-statement-from-nodes (copy-node subject)
                                                    (copy-node predicate)
                                                    (copy-node object)
                                                    world) model))

;;; TODO: find with options, requires hashes above

(defmacro define-get-function (what first second)
  `(defun ,(symbolicate 'model-get- what) (,first ,second &optional (model *model*))
     (let ((ret-val (,(symbolicate '%model-get- what) (get-pointer model 'model)
                      (get-pointer ,first 'node) (get-pointer ,second 'node))))
       (if (null-pointer-p ret-val)
           (error 'redland-error :format-control "Error in ~a retrieval function"
                  :format-arguments (list ',what))
           (wrap-pointer ret-val 'node-iterator (get-world model))))))

(define-get-function sources arc target)
(define-get-function arcs source target)
(define-get-function targets source arc)

(defmacro define-single-get-function (what first second)
  `(defun ,(symbolicate 'model-get- what) (,first ,second &optional (model *model*))
     (let ((ret-val (,(symbolicate '%model-get- what) (get-pointer model 'model)
                      (get-pointer ,first 'node) (get-pointer ,second 'node))))
       (if (null-pointer-p ret-val)
           nil
           (wrap-pointer ret-val 'node (get-world model))))))

(define-single-get-function source arc target)
(define-single-get-function arc source target)
(define-single-get-function target source arc)

(defun model-get-arcs-in (node &optional (model *model*))
  (let ((ret-val (%model-get-arcs-in (get-pointer model 'model) (get-pointer node 'node))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Error in get-arcs-in function.")
        (wrap-pointer ret-val 'node-iterator (get-world model)))))

(defun model-get-arcs-out (node &optional (model *model*))
  (let ((ret-val (%model-get-arcs-out (get-pointer model 'model) (get-pointer node 'node))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Error in get-arcs-in function.")
        (wrap-pointer ret-val 'node-iterator (get-world model)))))

;;; submodels?
;;; print? how to handle FILE*, anyway?

(defun model-context-add-statement (context statement &optional (model *model*))
  (let ((ret-code (%model-context-add-statement (get-pointer model 'model) (get-pointer context 'node) (get-pointer statement 'statement))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-context-add-statements (context statements &optional (model *model*))
  (let ((ret-code (%model-context-add-statements (get-pointer model 'model) (get-pointer context 'node) (get-pointer statements 'statements))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-context-remove-statement (context statement &optional (model *model*))
  (let ((ret-code (%model-context-remove-statement (get-pointer model 'model) (get-pointer context 'node) (get-pointer statement 'statement))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-context-remove-statements (context &optional (model *model*))
  (let ((ret-code (%model-context-remove-statements (get-pointer model 'model) (get-pointer context 'node))))
    (if (zerop ret-code)
        t
        (error 'redland-statement-add-error))))

(defun model-context-as-stream (context &optional (model *model*))
  (let ((ret-val (%model-context-as-stream (get-pointer model 'model) (get-pointer context 'node))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Error in model-context-as-stream function.")
        (wrap-pointer ret-val 'statement-stream (get-world model)))))

(defun model-contains-context (context &optional (model *model*))
  (not (zerop (%model-contains-context (get-pointer model 'model) (get-pointer context 'node)))))

(defun model-query-execute (query &optional (model *model*))
  (let ((ret-val (%model-query-execute (get-pointer model 'model) (get-pointer query 'query))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Error in model-query-execute function.")
        (wrap-pointer ret-val 'query-results (get-world model)))))

(defun model-sync (&optional (model *model*))
  (let ((ret-code (%model-sync (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to sync model to storage."))))

(defun model-get-storage (&optional (model *model*))
  (wrap-pointer (%model-get-storage (get-pointer model 'model)) 'storage (get-world model)))

(defun model-load (uri &key (model *model*) (name *null*) (mime-type *null*) (type-uri *null*))
  (let ((uri-object (if (stringp uri)
                        (make-uri uri)
                        uri)))
    (let ((ret-code (%model-load (get-pointer model 'model) (get-pointer uri-object 'uri) name mime-type (get-pointer type-uri 'uri))))
      (if (zerop ret-code)
          t
          (error 'redland-error :format-control "Failed to load model from ~a"
                 :format-arguments (list uri))))))

;;; TODO: model to counted string

(defun model-to-string (&key (model *model*) (base-uri *null*)
                        (name *null*) (mime-type *null*) (type-uri *null*))
  (%model-to-string (get-pointer model 'model) (get-pointer base-uri 'uri) name
                    mime-type (get-pointer type-uri 'uri)))

(defun model-find-statements-in-context (statement context &optional (model *model*))
  (let ((ret-val (%model-find-statements-in-context (get-pointer model 'model)
                                                    (get-pointer statement 'statement)
                                                    (get-pointer context 'node))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Error in model-find-statements-in-context function.")
        (wrap-pointer ret-val 'statement-stream (get-world model)))))

(defun model-find-statements-in-context-with-nodes (&key (context *null*) (subject *null*) (predicate *null*) (object *null*) (world *world*) (model *model*))
  (model-find-statements-in-context (make-statement-from-nodes subject predicate object world)
                                    context model))

(defun model-get-contexts (&optional (model *model*))
  (let ((ret-val (%model-get-contexts (get-pointer model 'model))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Failure in get-context, perhaps contexts are unsupported in this model.")
        (wrap-pointer ret-val 'node-iterator (get-world model)))))

(defun model-get-feature (model feature)
  (wrap-pointer (%model-get-feature (get-pointer model 'model) (get-pointer feature 'node)) 'node (get-world model)))

(defun model-set-feature (model feature value)
  (let ((result (%model-set-feature (get-pointer model 'model) (get-pointer feature 'node)
                                    (get-pointer value 'node))))
    (cond ((plusp result) (signal-feature-error model feature t))
          ((zerop result) t)
          ((minusp result) (signal-feature-error model feature nil)))))

(defun model-transaction-commit (&optional (model *model*))
  (let ((ret-code (%model-transaction-commit (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-transaction-error :format-control "Failed to commit transaction."))))

(defun model-transaction-rollback (&optional (model *model*))
  (let ((ret-code (%model-transaction-rollback (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-transaction-error :format-control "Failed to rollback transaction."))))

(defun model-transaction-start (&optional (model *model*))
  (let ((ret-code (%model-transaction-start (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-transaction-error :format-control "Failed to start transaction."))))

;;; handles, what are those?

;;; Nodes

(defun make-private-node (&optional (world *world*))
  (let ((new-node (%new-node (get-pointer world 'world))))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-uri-string (uri-string &optional (world *world*))
  (let ((new-node (%new-node-from-uri-string (get-pointer world 'world) uri-string)))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-uri (uri &optional (world *world*))
  (let ((new-node (%new-node-from-uri (get-pointer world 'world) (get-pointer uri 'uri))))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-uri-local-name (uri local-name &optional (world *world*))
  (let ((new-node (%new-node-from-uri-local-name (get-pointer world 'world)
                                                 (get-pointer uri 'uri)
                                                 local-name)))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-normalised-uri-string (uri-string source-uri base-uri &optional (world *world*))
  (let ((new-node (%new-node-from-normalised-uri-string (get-pointer world 'world) uri-string source-uri base-uri)))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-literal (string &key (world *world*) (language *null*) (is-wf-xml nil))
  (let ((new-node (%new-node-from-literal (get-pointer world 'world) string language (if is-wf-xml 1 0))))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-typed-literal (string &key (world *world*) (language *null*) (datatype-uri *null*))
  (let ((new-node (%new-node-from-typed-literal (get-pointer world 'world)
                                                string
                                                language (get-pointer datatype-uri 'uri))))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun make-node-from-blank-identifier (identifier &optional (world *world*))
  (let ((new-node (%new-node-from-blank-identifier (get-pointer world 'world) identifier)))
    (if (null-pointer-p new-node)
        (signal-construction-error 'node)
        (wrap-pointer new-node 'node world))))

(defun copy-node (node)
  (when node
    (let ((new-node (%new-node-from-node (get-pointer node 'node))))
      (if (null-pointer-p new-node)
          (signal-construction-error 'node)
          (wrap-pointer new-node 'node (get-world node))))))

(defun node-get-uri (node)
  (wrap-shared-pointer (%node-get-uri (get-pointer node 'node)) 'uri (get-world node)))

(defun node-get-type (node)
  (%node-get-type (get-pointer node 'node)))

(defun node-get-literal-value (node)
  (%node-get-literal-value (get-pointer node 'node)))

;;; TODO: some more node retrievals

(defun node-get-li-ordinal (node)
  (let ((ret-val
         (%node-get-li-ordinal (get-pointer node 'node))))
    (unless (= ret-val -1)
      ret-val)))

(defun node-get-blank-identifier (node)
  (%node-get-blank-identifier (get-pointer node 'node)))

(defun node-is-resource-p (node)
  (not (zerop (%node-is-resource (get-pointer node 'node)))))

(defun node-is-literal-p (node)
  (not (zerop (%node-is-literal (get-pointer node 'node)))))

(defun node-is-blank-p (node)
  (not (zerop (%node-is-blank (get-pointer node 'node)))))

(defun node-to-string (node)
  (%node-to-string (get-pointer node 'node)))

(defun node-equals-p (node1 node2)
  (zerop (%node-equals (get-pointer node1 'node) (get-pointer node2 'node))))

;;; Parsers

;;; skipping some function

(defun parser-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer))
    (let ((retval (%parser-enumerate (get-pointer world 'world) counter name label)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer)))))))

(defun parser-guess-name (&key (world *world*)
                          (mime-type *null*)
                          (buffer *null*)
                          (identifier *null*))
  (%parser-guess-name world mime-type buffer identifier))

(defun make-parser (name &key (world *world*) (mime-type *null*) (type-uri *null*))
  (let ((new-parser (%new-parser (get-pointer world 'world) name mime-type (get-pointer type-uri 'uri))))
    (if (null-pointer-p new-parser)
        (signal-construction-error 'parser)
        (wrap-pointer new-parser 'parser world))))

(defun parse-as-stream (parser uri &optional (base-uri *null*))
  (wrap-pointer (%parser-parse-as-stream (get-pointer parser 'parser) (get-pointer uri 'uri)
                                         (get-pointer base-uri 'uri))
                'statement-stream (get-world parser)))

(defun parse-into-model (parser uri &key (model *model*) (base-uri *null*))
  (let ((ret-code (%parser-parse-into-model (get-pointer parser 'parser)
                                            (get-pointer uri 'uri)
                                            (get-pointer base-uri 'uri)
                                            (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-error "Failure in parse-into-model"))))

(defun parse-string-as-stream (parser string &optional (base-uri *null*))
  (wrap-pointer (%parser-parse-as-stream (get-pointer parser 'parser) string
                                         (get-pointer base-uri 'uri))
                'statement-stream (get-world parser)))

(defun parse-string-into-model (parser string &key (model *model*) (base-uri *null*))
  (let ((ret-code (%parser-parse-into-model (get-pointer parser 'parser)
                                            string
                                            (get-pointer base-uri 'uri)
                                            (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-error "Failure in parse-string-into-model"))))

(defun parser-get-feature (parser feature)
  (wrap-pointer (%parser-get-feature (get-pointer parser 'parser) (get-pointer feature 'node)) 'node (get-world parser)))

(defun parser-set-feature (parser feature value)
  (let ((result (%parser-set-feature (get-pointer parser 'parser) (get-pointer feature 'node)
                                    (get-pointer value 'node))))
    (cond ((plusp result) (signal-feature-error parser feature t))
          ((zerop result) t)
          ((minusp result) (signal-feature-error parser feature nil)))))

;;; skipping namespaces

;;; Query

(defun make-query (query-string &key (world *world*) (name "sparql") (uri *null*) (base-uri *null*))
  (let ((new-query (%new-query (get-pointer world 'world)
                               name (get-pointer uri 'uri)
                               query-string (get-pointer base-uri 'uri))))
    (if (null-pointer-p new-query)
        (signal-construction-error 'query)
        (wrap-pointer new-query 'query world))))

(defun copy-query (old-query)
  (let ((new-query (%new-query-from-query (get-pointer old-query 'query))))
    (if (null-pointer-p new-query)
        (signal-construction-error 'query)
        (wrap-pointer new-query 'query (get-world old-query)))))

(defun query-execute (query &optional (model *model*))
  (let ((ret-val (%query-execute (get-pointer query 'query) (get-pointer model 'model))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Failed to execute query")
        (wrap-pointer ret-val 'query-results (get-world model)))))

(defun query-get-limit (query)
  (%query-get-limit (get-pointer query 'query)))

(defun query-set-limit (query limit)
  (let ((ret-code (%query-set-limit (get-pointer query 'query) limit)))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to set query limit"))))

(defun query-get-offset (query)
  (%query-get-offset (get-pointer query 'query)))

(defun query-set-offset (query offset)
  (let ((ret-code (%query-set-offset (get-pointer query 'query) offset)))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to set query offset"))))

;;; Query results

(defun query-results-as-stream (query-results)
  (unless (query-results-is-graph-p query-results)
    (error 'redland-error :format-control "Tried to convert non-graph query results to stream"))
  (let ((ret-val (%query-results-as-stream (get-pointer query-results 'query-results))))
    (if (null-pointer-p ret-val)
        (error 'redland-error :format-control "Failed to convert query-results to stream")
        (wrap-pointer ret-val 'statement-stream (get-world query-results)))))

(defun query-results-get-count (query-results)
  (%query-results-get-count (get-pointer query-results 'query-results)))

(defun query-results-next (query-results)
  (zerop (%query-results-next (get-pointer query-results 'query-results))))

(defun query-results-finished (query-results)
  (not (zerop (%query-results-finished (get-pointer query-results 'query-results)))))

(defun query-results-get-bindings (query-results)
  (assert (not (zerop (%query-results-is-bindings (get-pointer query-results 'query-results)))))
  (unless (query-results-finished query-results)
    (let ((bindings-count (query-results-get-bindings-count query-results)))
      (with-foreign-pointer (names (foreign-type-size :pointer))
        (with-foreign-objects ((values :pointer bindings-count))
          (let ((ret-code (%query-results-get-bindings (get-pointer query-results 'query-results) names values)))
            (unless (zerop ret-code)
              (error 'redland-error :format-control "Failure to assign query result bindings")))
          (iter (with name-array = (mem-ref names :pointer))
                (for i below bindings-count)
                (collect (cons (foreign-string-to-lisp (mem-aref name-array :pointer i))
                               (wrap-pointer (mem-aref values :pointer i)
                                             'node (get-world query-results))))))))))

(defun query-results-get-binding-value (query-results offset)
  (wrap-pointer (%query-results-get-binding-value (get-pointer query-results 'query-results) offset) 'node (get-world query-results)))

(defun query-results-get-binding-name (query-results offset)
  (%query-results-get-binding-name (get-pointer query-results 'query-results) offset))

(defun query-results-get-binding-value-by-name (query-results name)
  (wrap-pointer (%query-results-get-binding-value-by-name (get-pointer query-results 'query-results) name) 'node (get-world query-results)))

(defun query-results-get-bindings-count (query-results)
  (%query-results-get-bindings-count (get-pointer query-results 'query-results)))

(defun query-results-to-string (query-results format-uri &optional (base-uri *null*))
  (%query-results-to-string (get-pointer query-results 'query-results) (get-pointer format-uri 'uri) (get-pointer base-uri 'uri)))

(defun query-results-to-file (query-results name format-uri &optional (base-uri *null*))
  (%query-results-to-file (get-pointer query-results 'query-results) (namestring name)
                          (get-pointer format-uri 'uri) (get-pointer base-uri 'uri)))

(defun query-results-is-bindings-p (query-results)
  (not (zerop (%query-results-is-bindings (get-pointer query-results 'query-results)))))

(defun query-results-is-boolean-p (query-results)
  (not (zerop (%query-results-is-boolean (get-pointer query-results 'query-results)))))

(defun query-results-is-graph-p (query-results)
  (not (zerop (%query-results-is-graph (get-pointer query-results 'query-results)))))

(defun query-results-is-syntax-p (query-results)
  (not (zerop (%query-results-is-syntax (get-pointer query-results 'query-results)))))

(defun query-results-get-boolean (query-results)
  (let ((result (%query-results-get-boolean (get-pointer query-results 'query-results))))
    (cond ((> result 0) t)
          ((= result 0) nil)
          ((< result 0) :error-or-finished))))

(defun make-query-results-formatter (query-results &key (name *null*) (uri *null*))
  (wrap-pointer (%new-query-results-formatter (get-pointer query-results 'query-results)
                                              name
                                              (get-pointer uri 'uri))
                'query-results-formatter (get-world query-results)))

(defun make-query-results-formatter-by-mime-type (query-results mime-type)
  (wrap-pointer (%new-query-results-formatter-by-mime-type (get-pointer query-results 'query-results)
                                                           mime-type)
                'query-results-formatter (get-world query-results)))

(defun query-results-format-check (mime-type &key (name *null*) (uri *null*) (world *world*))
  (%query-results-formats-check (get-pointer world 'world) name
                                (get-pointer uri 'uri) mime-type))

(defun query-results-formats-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer)
                         (uri-string :pointer)
                         (mime-type :pointer))
    (let ((retval (%query-results-formats-enumerate (get-pointer world 'world) counter name label uri-string mime-type)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer))
              (foreign-string-to-lisp (mem-ref uri-string :pointer))
              (foreign-string-to-lisp (mem-ref mime-type :pointer)))))))

;;; Serialization

(defun serializer-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer))
    (let ((retval (%serializer-enumerate (get-pointer world 'world) counter name label)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer)))))))

(defun make-serializer (name &key (world *world*) (mime-type *null*) (type-uri *null*))
  (let ((new-serializer (%new-serializer (get-pointer world 'world) name mime-type (get-pointer type-uri 'uri))))
    (if (null-pointer-p new-serializer)
        (signal-construction-error 'serializer)
        (wrap-pointer new-serializer 'serializer world))))

(defun serialize-model-to-file (serializer name &key (base-uri *null*) (model *model*))
  (let ((ret-code (%serializer-serialize-model-to-file (get-pointer serializer 'serializer) name
                                                       (get-pointer base-uri 'uri) (get-pointer model 'model))))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to serialize model to file ~a"
               :format-arguments (list name)))))

(defun serialize-model-to-string (serializer &key (base-uri *null*) (model *model*))
  (%serializer-serialize-model-to-string (get-pointer serializer 'serializer)
                                         (get-pointer base-uri 'uri) (get-pointer model 'model)))

(defun serialize-stream-to-file (serializer name stream &key (base-uri *null*))
  (let ((ret-code (%serializer-serialize-stream-to-file (get-pointer serializer 'serializer) name
                                                        (get-pointer base-uri 'uri) (get-pointer stream 'statement-stream))))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to serialize stream to file ~a"
               :format-arguments (list name)))))

(defun serialize-stream-to-string (serializer stream &key (base-uri *null*))
  (%serializer-serialize-stream-to-string (get-pointer serializer 'serializer)
                                          (get-pointer base-uri 'uri) (get-pointer stream 'statement-stream)))

(defun serializer-get-feature (serializer feature)
  (wrap-pointer (%serializer-get-feature (get-pointer serializer 'serializer) (get-pointer feature 'node)) 'node (get-world serializer)))

(defun serializer-set-feature (serializer feature value)
  (let ((result (%serializer-set-feature (get-pointer serializer 'serializer) (get-pointer feature 'node)
                                         (get-pointer value 'node))))
    (cond ((plusp result) (signal-feature-error serializer feature t))
          ((zerop result) t)
          ((minusp result) (signal-feature-error serializer feature nil)))))

(defun serilizer-set-namespace (serializer uri prefix)
  (let ((ret-code (%serializer-set-namespace (get-pointer serializer 'serializer)
                                             (get-pointer uri 'uri)
                                             prefix)))
    (if (zerop ret-code)
        t
        (error 'redland-error :format-control "Failed to set namespace ~a in serializer ~a to prefix ~a"
               :format-arguments (list uri serializer prefix)))))

;;; Statement

(defun make-statement (&optional (world *world*))
  (let ((new-statement (%new-statement (get-pointer world 'world))))
    (if (null-pointer-p new-statement)
        (signal-construction-error 'statement)
        (wrap-pointer new-statement 'statement world))))

(defun copy-statement (statement)
  (let ((new-statement (%new-statement-from-statement (get-pointer statement 'statement))))
    (if (null-pointer-p new-statement)
        (signal-construction-error 'statement)
        (wrap-pointer new-statement 'statement (get-world statement)))))

(defun make-statement-from-nodes (subject predicate object &optional (world *world*))
  (when subject
    (unown-pointer subject))
  (when predicate
    (unown-pointer predicate))
  (when object
    (unown-pointer object))
  (let ((new-statement (%new-statement-from-nodes (get-pointer world 'world)
                                                  (if subject
                                                      (get-pointer subject 'node)
                                                      *null*)
                                                  (if predicate
                                                      (get-pointer predicate 'node)
                                                      *null*)
                                                  (if object
                                                      (get-pointer object 'node)
                                                      *null*))))
    (if (null-pointer-p new-statement)
        (signal-construction-error 'statement)
        (wrap-pointer new-statement 'statement world))))

(defun make-template (&key (subject *null*) (predicate *null*) (object *null*) (world *world*))
  (make-statement-from-nodes subject predicate object world))

(defun statement-clear (statement)
  (%statement-clear (get-pointer statement 'statement)))

(defmacro statement-get-set (&rest what)
  (cons 'progn
        (iter (for w in what)
              (appending
               (list `(defun ,(symbolicate 'statement-get- w) (statement)
                        (wrap-shared-pointer (,(symbolicate '%statement-get- w) (get-pointer statement 'statement))
                                             'node (get-world statement)))
                     `(defun ,(symbolicate 'statement-set- w) (statement ,w)
                        (unown-pointer ,w)
                        (,(symbolicate '%statement-set- w) (get-pointer statement 'statement) (get-pointer ,w 'node))))))))

(statement-get-set subject predicate object)

(defun statement-is-complete-p (statement)
  (not (zerop (%statement-is-complete (get-pointer statement 'statement)))))

(defun statement-to-string (statement)
  (%statement-to-string (get-pointer statement 'statement)))

(defun statement-equals-p (statement1 statement2)
  (not (zerop (%statement-equals (get-pointer statement1 'statement)
                                 (get-pointer statement2 'statement)))))

(defun statement-match-p (statement partial-statement)
  (not (zerop (%statement-match (get-pointer statement 'statement)
                                (get-pointer partial-statement 'statement)))))

;;; TODO: encode/decode

;;; Triple stores

;;; skipping implementation methods

(defun storage-enumerate (counter &optional (world *world*))
  (with-foreign-objects ((name :pointer)
                         (label :pointer))
    (let ((retval (%storage-enumerate (get-pointer world 'world) counter name label)))
      (when (zerop retval)
        (list (foreign-string-to-lisp (mem-ref name :pointer))
              (foreign-string-to-lisp (mem-ref label :pointer)))))))

(defun make-storage (storage-name name options &optional (world *world*))
  (let ((new-storage (%new-storage (get-pointer world 'world)
                                   storage-name name options)))
    (if (null-pointer-p new-storage)
        (signal-construction-error 'storage)
        (wrap-pointer new-storage 'storage world))))

(defmacro with-storage ((storage-name name options &optional (world '*world*)) &body body)
  `(let ((*storage* (make-storage ,storage-name ,name ,options ,world)))
     (unwind-protect (progn ,@body)
       (free-pointer *storage*))))

;;; TODO: storage with options, requires hashes

(defun copy-storage (old-storage)
  (let ((new-storage (%new-storage-from-storage (get-pointer old-storage 'storage))))
    (if (null-pointer-p new-storage)
        (signal-construction-error 'storage)
        (wrap-pointer new-storage 'storage (get-world old-storage)))))

(defun storage-get-feature (storage feature)
  (wrap-pointer (%storage-get-feature (get-pointer storage 'storage) (get-pointer feature 'node)) 'node (get-world storage)))

(defun storage-set-feature (storage feature value)
  (let ((result (%storage-set-feature (get-pointer storage 'storage) (get-pointer feature 'node)
                                    (get-pointer value 'node))))
    (cond ((plusp result) (signal-feature-error storage feature t))
          ((zerop result) t)
          ((minusp result) (signal-feature-error storage feature nil)))))

;;; Streams

;;; like with iterators, minimal interface

(defun stream-next (stream)
  (zerop (%stream-next (get-pointer stream 'statement-stream))))

(defun stream-endp (stream)
  (not (zerop (%stream-end (get-pointer stream 'statement-stream)))))

(defun stream-get-object (stream)
  (wrap-shared-pointer (%stream-get-object (get-pointer stream 'statement-stream)) 'statement (get-world stream)))

(defun make-stream-from-node-iterator (node-iterator statement field)
  (let ((new-stream (%new-stream-from-node-iterator
                     (get-pointer node-iterator 'node-iterator) (get-pointer statement 'statement) field)))
    (if (null-pointer-p new-stream)
        (signal-construction-error 'stream)
        (wrap-pointer new-stream 'statement-stream (get-world node-iterator)))))

;;; URI

(defun make-uri (uri-string &optional (world *world*))
  (let ((new-uri (%new-uri (get-pointer world 'world) uri-string)))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri world))))

(defun copy-uri (old-uri)
  (let ((new-uri  (%new-uri-from-uri (get-pointer old-uri 'uri))))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri (get-world old-uri)))))

(defun make-uri-from-uri-local-name (old-uri local-name)
  (let ((new-uri  (%new-uri-from-uri-local-name (get-pointer old-uri 'uri) local-name)))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri (get-world old-uri)))))

(defun uri-as-string (uri)
  (%uri-as-string (get-pointer uri 'uri)))

(defun uri-to-string (uri)
  (%uri-to-string (get-pointer uri 'uri)))

(defun uri-equals (uri1 uri2)
  (not (zerop (%uri-equals (get-pointer uri1 'uri)
                           (get-pointer uri2 'uri)))))

(defun is-file-uri-p (uri)
  (not (zerop (%uri-is-file-uri (get-pointer uri 'uri)))))

(defun uri-to-filename (uri)
  (%uri-to-filename (get-pointer uri 'uri)))

(defun make-uri-normalised-to-base (uri-string source-uri base-uri)
  (let ((new-uri  (%new-uri-normalised-to-base uri-string
                                               (get-pointer source-uri 'uri)
                                               (get-pointer base-uri 'uru))))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri (get-world source-uri)))))

(defun make-uri-relative-to-base (base-uri uri-string)
  (let ((new-uri  (%new-uri-relative-to-base (get-pointer base-uri 'uri)
                                             uri-string)))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri (get-world base-uri)))))

(defun make-uri-from-filename (filename &optional (world *world*))
  (let ((new-uri  (%new-uri-from-filename (get-pointer world 'world) (namestring filename))))
    (if (null-pointer-p new-uri)
        (signal-construction-error 'uri)
        (wrap-pointer new-uri 'uri world))))

(defun uri-compare (uri1 uri2)
  (%uri-compare (get-pointer uri1 'uri) (get-pointer uri2 'uri)))

;;; auxiliary

(defun get-all-enumerate (enumerate-fun &optional (world *world*))
  (iter (for i from 0)
        (for e = (funcall enumerate-fun i world))
        (while e)
        (collect e)))

