(defpackage #:sparql-macro
    (:use #:cl #:bpm #:iterate #:alexandria #:redland))

(in-package :sparql-macro)

;;; redland dependency is there to be able to transform nodes/uris to strings

;;; all IRIs should go through node/uri objects, no direct string insertion into output

;;; literal syntax is sketchy, expand as needed

(defun terminal-to-string (terminal)
  (with-output-to-string (*standard-output*)
    (etypecase terminal
      (string
         (princ "'''")
         (princ terminal);sanitize strings properly
         (princ "'''"))
      (number (princ terminal))
      (uri
         (princ "<")
         (princ (uri-as-string terminal))
         (princ ">"))
      (node
         (ecase (node-get-type terminal)
           (:resource (terminal-to-string (node-get-uri terminal)))
           (:literal (terminal-to-string (node-get-literal-value terminal)))
           (:blank (princ (node-get-blank-identifier terminal))))))))

;;; transformation functions return as values result code and free variables within if no variables
;;; code can be immediately executed and replaced with string... maybe

;;; no PREFIX or BASE, since that should be handled by uri created by make-uri-from-uri-local-name,
;;; as above

(def! transform-sparql nil
  (values nil nil))

(def transform-sparql ~
  (values '(princ "a") nil))

(def transform-sparql _terminal
  (where (symbolp _terminal))
  (cond ((eql (char (symbol-name _terminal) 0) #\?)
         (values `(princ ,(string-downcase _terminal)) nil))
        (t (values `(princ ,_terminal) (list _terminal)))))

(def transform-sparql _collection
  (where (vectorp _collection))
  (let ((vars nil))
    (let ((code `(progn
                   (princ "( ")
                   ,@(iter (for v in-vector _collection)
                           (multiple-value-bind (code code-vars)
                               (transform-sparql v)
                             (collect code)
                             (collect '(princ " "))
                             (appendf vars code-vars)))
                   (princ ")"))))
      (values code vars))))

(def transform-sparql _terminal
  (where (atom _terminal))
  (values (terminal-to-string _terminal) nil))

;;; @ marks triples

;;;; no idea how to express patterns properly, so explicit tests for four cases
(def! transform-po nil
  (values nil nil))

(def transform-po (_p _o)
  (where (and (atom _p) (atom _o)))
  (multiple-value-bind (p-code p-vars) (transform-sparql _p)
    (multiple-value-bind (o-code o-vars) (transform-sparql _o)
      (values `(progn ,p-code (princ " ") ,o-code)
              (append p-vars o-vars)))))

(defun transform-olist (olist)
  (let ((vars nil))
   (let ((o-code (iter (for (o . on) on olist)
                       (multiple-value-bind (o-code o-vars) (transform-sparql o)
                         (collect o-code)
                         (appendf vars o-vars))
                       (when on (collect `(princ " , "))))))
     (values o-code vars))))

(def transform-po (_p _o)
  (where (and (atom _p) (listp _o)))
  (let ((vars nil))
   (multiple-value-bind (p-code p-vars) (transform-sparql _p)
     (appendf vars p-vars)
     (let ((code `(progn ,p-code
                         (princ " ")
                         ,@(multiple-value-bind (o-code o-vars) (transform-olist _o)
                             (appendf vars o-vars)
                             o-code))))
       (values code vars)))))

(def transform-po _po
  (where (listp _po))
  (let ((vars nil)
        (code nil))
    (iter (for ((p . o) . po-next) on _po)
          (multiple-value-bind (p-code p-vars) (transform-sparql p)
            (multiple-value-bind (o-code o-vars) (transform-olist (if (listp (car o))
                                                                      (car o)
                                                                      o))
              (appendf vars p-vars o-vars)
              (appendf code (list p-code))
              (appendf code '((princ " ")))
              (appendf code o-code)))
          (when po-next (appendf code '((princ " ; ")))))
    (values (cons 'progn code) vars)))

(def transform-sparql (@ _s . _po)
  (multiple-value-bind (s-code s-vars) (transform-sparql _s)
    (multiple-value-bind (po-code po-vars) (transform-po _po)
      (values (list 'progn s-code '(princ " ") po-code '(princ " . "))
              (append s-vars po-vars)))))

(def transform-sparql ($ . _contents)
  (let ((code nil)
        (vars nil))
    (iter (for c in _contents)
          (multiple-value-bind (c-code c-vars) (transform-sparql c)
            (push c-code code)
            (setf vars (remove-duplicates (append vars c-vars)))))
    (values `(progn (princ "{") ,@(nreverse code) (princ "}")) vars)))

(def transform-sparql (& . _blank)
  (if (null _blank)
      (values '(princ "[] ") nil)
      (multiple-value-bind (po-code po-vars) (transform-po _blank)
        (values `(progn (princ "[ ") ,po-code (princ " ]"))
                po-vars))))

(def transform-sparql (opt . _group)
  (multiple-value-bind (g-code g-vars) (transform-sparql _group)
    (values `(progn (princ "OPTIONAL { ") ,g-code (princ " }"))
            (remove-duplicates g-vars))))

(def transform-sparql (alt . _group)
  (let ((code nil) (vars nil))
    (iter (for g in _group)
          (multiple-value-bind (g-code g-vars) (transform-sparql g)
            (push g-code code)
            (setf vars (remove-duplicates (append vars g-vars)))))
    (values `(progn (princ "{ ")
                    ,@(iter (for (g . gn) on (nreverse code))
                            (collect '(princ " { "))
                            (collect g)
                            (collect '(princ " } "))
                            (when gn (collect '(princ " UNION "))))
                    (princ " }"))
            vars)))

(def transform-sparql (graph _graph . _group)
  (multiple-value-bind (g-code g-vars) (transform-sparql (if (eql (car _group) '$)
                                                             _group
                                                             (list '$ _group)))
    (multiple-value-bind (gr-code gr-vars) (transform-sparql _graph)
     (values `(progn (princ "GRAPH ") ,gr-code (princ " ") ,g-code)
             (remove-duplicates (append g-vars gr-vars))))))

(def transform-sparql (select _vars _where . _mods)
  (multiple-value-bind (w-code w-vars) (transform-sparql (if (eql (car _where) '$)
                                                             _where
                                                             (list '$ _where)))
    (values `(progn
               (princ "SELECT")
               ,@(when (getf _mods :distinct)
                   '((princ " DISTINCT ")))
               ,@(when (getf _mods :reduced)
                   '((princ " REDUCED ")))
               ,@(iter (for v in _vars)
                       (collect '(princ " "))
                       (assert (and (symbolp v)
                                    (char= (char (symbol-name v) 0) #\?)))
                       (collect `(princ ,(string-downcase v))))
               (princ " WHERE ")
               ,w-code
               ,@(when (getf _mods :order-by);more: lists, directions
                 `((princ " ORDER BY ")
                   (princ ,(string-downcase (getf _mods :order-by)))))
               ,@(when (getf _mods :limit)
                   `((princ " LIMIT ")
                     (princ ,(getf _mods :limit))))
               ,@(when (getf _mods :offset)
                   `((princ " OFFSET ")
                     (princ ,(getf _mods :offset)))))
            w-vars)))

(def transform-sparql (construct _template _where . _mods)
  (multiple-value-bind (w-code w-vars) (transform-sparql _where)
    (values `(progn
               (princ "CONSTRUCT")
               ,(transform-sparql (if (eql (car _template) '$)
                                      _template
                                      (list '$ _template)))
               (princ " WHERE ")
               ,w-code
               ,@(when (getf _mods :limit)
                   `((princ " LIMIT ")
                     (princ ,(getf _mods :limit))))
               ,@(when (getf _mods :offset)
                   `((princ " OFFSET ")
                     (princ ,(getf _mods :offset)))))
            w-vars)))

(def transform-sparql (ask _where)
  (multiple-value-bind (w-code w-vars) (transform-sparql _where)
    (values `(progn
               (princ "ASK")
               ,w-code)
            w-vars)))

(def! transform-filter _endpoint
  (where (symbolp _endpoint))
  (if (char= (char (symbol-name _endpoint) 0) #\?)
      (values `(princ ,(string-downcase _endpoint)) nil)
      (values _endpoint (list _endpoint))))

(def transform-filter _endpoint
  (where (atom _endpoint))
  (values `(princ ,(terminal-to-string _endpoint)) nil))

(defmacro define-unary-filter-op (op expansion)
  `(def transform-filter (,op _a)
     (multiple-value-bind (a-code a-vars) (transform-filter _a)
       (values `(progn (princ "(") (princ ,,expansion) ,a-code (princ ")"))
               a-vars))))

(define-unary-filter-op not "! ")

(define-unary-filter-op + "+ ")

(define-unary-filter-op - "- ")

(defmacro define-unary-filter-fun (fun expansion)
  `(def transform-filter (,fun _a)
     (multiple-value-bind (a-code a-vars) (transform-filter _a)
       (values `(progn (princ ,,expansion) (princ "(") ,a-code (princ ")"))
               a-vars))))

(define-unary-filter-fun boundp "BOUND")

(define-unary-filter-fun is-iri-p "isIRI")

(define-unary-filter-fun is-uri-p "isURI")

(define-unary-filter-fun is-blank-p "isBLANK")

(define-unary-filter-fun is-literal-p "isLITERAL")

(define-unary-filter-fun str "STR")

(define-unary-filter-fun lang "LANG")

(define-unary-filter-fun datatype "DATATYPE")

(defmacro define-binary-filter-op (op expansion)
  `(def transform-filter (,op _a _b)
     (multiple-value-bind (a-code a-vars) (transform-filter _a)
       (multiple-value-bind (b-code b-vars) (transform-filter _b)
         (values `(progn (princ "(") ,a-code (princ " ") (princ ,,expansion) (princ " ") ,b-code (princ ")"))
                 (remove-duplicates (append a-vars b-vars)))))))

(define-binary-filter-op or "||")
(define-binary-filter-op and "&&")
(define-binary-filter-op = "=")
(define-binary-filter-op != "!=")
(define-binary-filter-op < "<")
(define-binary-filter-op > ">")
(define-binary-filter-op >= ">=")
(define-binary-filter-op <= "<=")
(define-binary-filter-op * "*")
(define-binary-filter-op / "/")
(define-binary-filter-op + "+")
(define-binary-filter-op - "-")

(define-unary-filter-fun same-term-p "sameTERM")

(defmacro define-binary-filter-fun (fun expansion)
  `(def transform-filter (,fun _a _b)
     (multiple-value-bind (a-code a-vars) (transform-filter _a)
       (multiple-value-bind (b-code b-vars) (transform-filter _b)
        (values `(progn (princ ,,expansion) (princ "(") ,a-code (princ ",") ,b-code (princ ")"))
                (remove-duplicates (append a-vars b-vars)))))))

(define-binary-filter-fun regex "REGEX")

(defmacro define-trinary-filter-fun (fun expansion)
  `(def transform-filter (,fun _a _b _c)
     (multiple-value-bind (a-code a-vars) (transform-filter _a)
       (multiple-value-bind (b-code b-vars) (transform-filter _b)
         (multiple-value-bind (c-code c-vars) (transform-filter _c)
           (values `(progn (princ ,,expansion) (princ "(") ,a-code (princ ",") ,b-code
                           (princ ",") ,c-code (princ ")"))
                   (remove-duplicates (append a-vars b-vars c-vars))))))))

(define-trinary-filter-fun regex "REGEX")

(def transform-sparql (filter _filter)
  (multiple-value-bind (f-code f-vars) (transform-filter _filter)
    (values `(progn
               (princ "FILTER ")
               ,f-code)
            f-vars)))

;allow arbitrary code insertion, allowing macros and things, doesn't handle additional variables
;though
(def transform-sparql _something-else
  _something-else)

(defmacro sparql (query &rest local-bindings)
  (multiple-value-bind (code vars) (transform-sparql query)
    `(with-output-to-string (*standard-output*)
       (let* (,@(iter (for b in local-bindings)
                     (collect b)))
         (let (,@(iter (for v in (remove-duplicates vars))
                       (collect (list v `(terminal-to-string ,v)))))
           ,code)))))
