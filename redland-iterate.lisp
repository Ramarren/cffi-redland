(in-package :redland)

(defmacro-driver (FOR var IN-REDLAND-STREAM redland-stream)
  (let ((str (gensym))
        (first (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,str = ,redland-stream)
       (with ,first = t)
       (,kwd ,var next (progn
                         (if ,first
                             (setf ,first nil)
                             (stream-next ,str))
                         (when (stream-endp ,str) (terminate))
                         (stream-get-object ,str))))))

(defmacro-driver (FOR var IN-REDLAND-ITERATOR redland-iterator)
  (let ((itr (gensym))
        (first (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,itr = ,redland-iterator)
       (with ,first = t)
       (,kwd ,var next (progn
                         (if ,first
                             (setf ,first nil)
                             (iterator-next ,itr))
                         (when (iterator-endp ,itr) (terminate))
                         (iterator-get-object ,itr))))))

(defmacro-driver (FOR var IN-MODEL model)
  (let ((str (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,str = (model-as-stream ,model))
            (,kwd ,var in-redland-stream ,str))))

(defmacro-driver (FOR var IN-QUERY-RESULTS query-results)
  (let ((qr (gensym))
        (first (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,qr = ,query-results)
       (with ,first = t)
       (,kwd ,var next (progn
                         (if ,first
                             (setf ,first nil)
                             (query-results-next ,qr))
                         (when (query-results-finished ,qr) (terminate))
                         (query-results-get-bindings ,qr))))))
