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

(defun object-list (objects stream)
  (iter (for (o . on) on objects)
        (princ-terminal o stream)
        (when on (princ ", " stream))))

(defun triple (subject predicate object)
  (with-output-to-string (stream)
    (princ-terminal subject stream)
    (princ " " stream)
    (princ-terminal predicate stream)
    (princ " " stream)
    (if (atom object)
        (princ-terminal object stream)
        (object-list object stream))
    (princ " " stream)))

(defun po-list (predicates-and-objects stream)
  (iter (generate po in predicates-and-objects)
        (for i from 1)
        (with l = (/ (length predicates-and-objects) 2))
        (princ-terminal (next po) stream)
        (princ " " stream)
        (if (atom (next po))
            (princ-terminal po stream)
            (object-list po stream))
        (print i)
        (print l)
        (unless (= i l)
          (princ "; " stream))))

(defun predicate-object-list (subject &rest predicates-and-objects)
  (assert (evenp (length predicates-and-objects)))
  (with-output-to-string (stream)
    (princ-terminal subject stream)
    (princ " " stream)
    (po-list predicates-and-objects stream)))

(defun group (&rest triples)
  (with-output-to-string (stream)
    (princ "{" stream)
    (terpri stream)
    (iter (for triple in triples)
          (princ triple stream)
          (princ "." stream)
          (terpri stream))
    (princ "}" stream)))

(defun collection (&rest contents)
  (with-output-to-string (stream)
    (princ "(" stream)
    (iter (for (c . cn) on contents)
          (princ-terminal c stream)
          (when cn (princ " " stream)))
    (princ ")" stream)))

(declaim (inline {} t. po))
(defun {} (&rest triples)
  (group triples))
(defun t. (subject predicate object)
  (triple subject predicate object))
(defun po (subject &rest predicates-and-objects)
  (apply #'predicate-object-list subject predicates-and-objects))

(defun a ()
  "a")

(defun blank (&rest predicates-and-objects)
  (assert (evenp (length predicates-and-objects)))
  (if (null predicates-and-objects)
      "[]"
      (with-output-to-string (stream)
        (princ "[ " stream)
        (po-list predicates-and-objects stream)
        (princ " ]" stream))))

(defun rdf-string (string)
  (concatenate 'string "\"" string "\""))

(defun filter (filter)
  (with-output-to-string (stream)
    (princ "FILTER " stream)
    (princ (build-filter-expression filter) stream)))

(defun graph (graph body)
  (with-output-to-string (stream)
    (princ "GRAPH " stream)
    (princ-terminal graph stream)
    (princ body stream)))

;;; build filter expression

;;; pattern matching would help, but don't want to draw in more dependencies
(defun build-filter-expression (filter-expression)
  (with-output-to-string (stream)
    (labels ((p (&rest args)
               (iter (for a in args)
                     (princ a stream))))
      (if (atom filter-expression)
          (princ-terminal filter-expression stream)
          (let ((args (mapcar #'build-filter-expression (cdr filter-expression))))
                  (p "(")
            (ecase (car filter-expression)
              (not (p "! " (car args)))
              (+ (if (= (length args) 1)
                     (p "+ " (car args))
                     (iter (for (a . e) on args)
                           (if e (p a " + ") (p a)))))
              (- (if (= (length args) 1)
                     (p "- " (car args))
                     (iter (for (a . e) on args)
                           (if e (p a " - ") (p a)))))
              (* (iter (for (a . e) on args)
                       (if e (p a " * ") (p a))))
              (/ (iter (for (a . e) on args)
                       (if e (p a " / ") (p a))))
              ((> < >= <= = !=)
                 (p (car args) " "
                    (symbol-name (car filter-expression)) " "
                    (cadr args)))
              (or (p (car args) " || " (cadr args)))
              (and (p (car args) " && " (cadr args))))
            (p ")"))))))

;;; CONSTRUCT

(defun construct (template where &key order-by limit offset)
  (with-output-to-string (stream)
    (princ "CONSTRUCT {" stream)
    (assert (mod (length template) 3))
    (iter (for (p o s . rest) on template by #'cdddr)
          (princ-terminal p stream)
          (princ " " stream)
          (princ-terminal o stream)
          (princ " " stream)
          (princ-terminal s stream)
          (when rest
            (princ " . " stream)))
    (princ "}" stream)
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

;;; ASK

(defun ask (what)
  (with-output-to-string (stream)
    (princ "ASK " stream)
    (princ what stream)))
