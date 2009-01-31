(in-package :redland)

;;; in-query-result iterate driver allocates names of bindings for every iteration, and is not very
;;; convenient anyway

;;; do-query macro

(defun normalize-bindings-designator (bindings-list)
  (iter (for b in bindings-list)
        (if (symbolp b)
            (collect (cons b (string-downcase b)))
            (destructuring-bind (binding name) b
              (collect (cons binding name))))))

(defun do-query-execute-query (query)
  (etypecase query
    (string (query-execute (make-query query)))
    (query (query-execute query))
    (query-results query)))

(defun binding-offsets (b-names query-results)
  (iter (with a = (make-array (length b-names) :initial-element -1 :element-type 'fixnum))
        (for k from 0 below (query-results-get-bindings-count query-results))
        (for n = (query-results-get-binding-name query-results k))
        (when-let (pos (position n b-names :test #'string=))
          (setf (aref a pos) k))
        (finally
         (if (notany #'minusp a)
             (return a)
             (error 'redland-error :format-control "There is no binding ~a in query result."
                    :format-arguments (list (nth (position -1 a) b-names)))))))

(defmacro do-query (query (&rest bindings) &body body)
  (let ((bindings (normalize-bindings-designator bindings)))
    (with-unique-names (query-res bindings-array)
      `(let ((,query-res (do-query-execute-query ,query))
             ,@(iter (for (b . nil) in bindings)
                     (collect b)))
         (unless (query-results-is-bindings-p ,query-res)
           (error 'redland-error "Can't do-query non-bindings result."))
         (let ((,bindings-array (binding-offsets ',(mapcar #'cdr bindings) ,query-res)))
           (iter (until (query-results-finished ,query-res))
                 (setf ,@(iter (for (b . nil) in bindings)
                               (for i from 0)
                               (collect b)
                               (collect `(query-results-get-binding-value ,query-res
                                                                          (aref ,bindings-array ,i)))))
                 ,@body
                 (query-results-next ,query-res)))))))
