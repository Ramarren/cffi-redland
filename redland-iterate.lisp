(in-package :redland)

(defmacro-driver (FOR var IN-REDLAND-STREAM redland-stream)
  (let ((str (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,str = ,redland-stream)
       (,kwd ,var next (progn (when (stream-endp ,str) (terminate))
                              (unless (first-iteration-p) (stream-next ,str))
                              (stream-get-object ,str))))))

(defmacro-driver (FOR var IN-REDLAND-ITERATOR redland-iterator)
  (let ((itr (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn
       (with ,itr = ,redland-iterator)
       (,kwd ,var next (progn (when (iterator-endp ,itr) (terminate))
                              (unless (first-iteration-p (iterator-next ,itr)))
                              (iterator-get-object ,itr))))))

(defmacro-driver (FOR var IN-MODEL model)
  (let ((str (gensym))
        (kwd (if generate 'generate 'for)))
    `(progn (with ,str = (model-as-stream ,model))
            (,kwd ,var in-redland-stream ,str))))