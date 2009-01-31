(in-package :redland)

;;; lisp syntax for sparql for redland
;;; without any planning
;;; with functions, how much speed does query construction needs anyway?

(defun princ-terminal (terminal stream)
  (etypecase terminal
    (string (princ terminal stream))
    (number (princ terminal stream))
    (symbol
       (princ "?" stream)
       (princ (string-downcase terminal) stream))
    (uri
       (princ "<" stream)
       (princ (uri-as-string terminal) stream)
       (princ ">" stream))
    (node
       (ecase (node-get-type terminal)
         (:resource (princ-terminal (node-get-uri terminal) stream))
         (:literal (princ (node-get-literal-value terminal) stream))
         (:blank (princ (node-get-blank-identifier terminal)))))))

(defun base (base-decl body)
  (with-output-to-string (stream)
    (princ "BASE " stream)
    (princ-terminal base-decl stream)
    (terpri stream)
    (princ body stream)))

(defun prefix (prefix-decls body)
  (with-output-to-string (stream)
    (iter (for (name prefix) in prefix-decls)
          (princ "PREFIX " stream)
          (princ name stream)
          (princ " " stream)
          (princ-terminal prefix stream)
          (terpri stream))
    (princ body stream)))

(defun select (vars where &key order-by limit offset distinct reduced)
  (with-output-to-string (stream)
    (princ "SELECT " stream)
    (when distinct (princ "DISTINCT " stream))
    (when reduced (princ "REDUCED " stream))
    (iter (for v in vars)
          (princ-terminal v stream)
          (princ " " stream))
    (terpri stream)
    (princ "WHERE " stream)
    (princ where stream)
    (when order-by
      (princ "ORDER BY " order-by)
      (terpri))
    (when limit
      (princ "LIMIT " limit))
    (when offset
      (princ "OFFSET " offset))))

(defun triple (subject predicate object)
  (with-output-to-string (stream)
    (princ-terminal subject stream)
    (princ " " stream)
    (princ-terminal predicate stream)
    (princ " " stream)
    (princ-terminal object stream)
    (princ " " stream)))

(defun group (&rest triples)
  (with-output-to-string (stream)
    (princ "{" stream)
    (terpri stream)
    (iter (for triple in triples)
          (princ triple stream)
          (princ "." stream)
          (terpri stream))
    (princ "}" stream)))

(defun filter (filter)
  (with-output-to-string (stream)
    (princ "FILTER (" stream)
    (princ filter stream)
    (princ ") " stream)))

(defun graph (graph body)
  (with-output-to-string (stream)
    (princ "GRAPH " stream)
    (princ-terminal graph stream)
    (princ body stream)))
