(in-package :redland)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-redland-libraries))

;;; Types

(defctype world-pointer :pointer "Pointer to Redland world class")
(defctype node-pointer :pointer "Pointer to Redland node class")
(defctype uri-pointer :pointer "Pointer to Redland URI class")
(defctype digest-pointer :pointer "Pointer to Redland content digest class")
(defctype digest-factory-pointer :pointer "Pointer to Redland digest factory class")
(defctype hash-pointer :pointer "Pointer to Redland hash class")
(defctype hash-cursor-pointer :pointer "Pointer to Redland hash cursor class")
(defctype new-string :pointer "String in newly allocated memory")
(defctype iterator-pointer :pointer "Pointer to Redland iterator class")
(defctype list-pointer :pointer "Pointer to Redland list class")
(defctype model-pointer :pointer "Pointer to Redland model class")
(defctype model-factory-pointer :pointer "Pointer to Redland model factory class")
(defctype storage-pointer :pointer "Pointer to Redland storage class")
(defctype storage-factory-pointer :pointer)
(defctype stream-pointer :pointer "Pointer to Redland stream class")
(defctype query-pointer :pointer)
(defctype query-factory-pointer :pointer)
(defctype query-results-pointer :pointer "Pointer to query results")
(defctype statement-pointer :pointer)
(defctype parser-pointer :pointer)
(defctype parser-factory-pointer :pointer)
(defctype query-results-formatter-pointer :pointer)
(defctype raptor-iostream-pointer :pointer)
(defctype serializer-pointer :pointer)
(defctype serializer-factory-pointer :pointer)
(defctype message-pointer :pointer)

(defmethod translate-from-foreign (value (type (eql 'new-string)))
  (prog1
      (unless (null-pointer-p value)
        (foreign-string-to-lisp value))
    (foreign-string-free value)))

;;; World

(defcfun (%new-world "librdf_new_world") world-pointer)

(defcfun (%free-world "librdf_free_world") :void (world world-pointer))

(defcfun (%world-open "librdf_world_open") :void (world world-pointer))

(defcfun (%world-set-error "librdf_world_set_error") :void
  (world world-pointer)
  (user-data :pointer)
  (error-handler :pointer))

(defcfun (%world-set-warning "librdf_world_set_warning") :void
  (world world-pointer)
  (user-data :pointer)
  (warning-handler :pointer))

(defcfun (%world-set-logger "librdf_world_set_logger") :void
  (world world-pointer)
  (user-data :pointer)
  (logger-callback :pointer));callback user-data, message->int (nonzero means handled)

(defcfun (%world-set-digest "librdf_world_set_digest") :void
  (world world-pointer)
  (name :string))

(defcfun (%world-get-feature "librdf_world_get_feature") node-pointer
  (world world-pointer)
  (feature uri-pointer))

(defcfun (%world-set-feature "librdf_world_set_feature") :int
  (world world-pointer)
  (feature uri-pointer)
  (value node-pointer))

;;; Concepts

(defcfun (%get-concept-resource-by-index "librdf_get_concept_resource_by_index") node-pointer
  (world world-pointer)
  (idx concepts-index))

(defcfun (%get-concept-uri-by-index "librdf_get_concept_uri_by_index") uri-pointer
  (world world-pointer)
  (idx concepts-index))

(defcfun (%get-concept-ms-namespace "librdf_get_concept_ms_namespace") uri-pointer
  (world world-pointer))

(defcfun (%get-concept-schema-namespace "librdf_get_concept_schema_namespace") uri-pointer
  (world world-pointer))

;;; Digests

(defcfun (%new-digest "librdf_new_digest") digest-pointer
  (world world-pointer)
  (name :string))

(defcfun (%free-digest "librdf_free_digest") :void
  (digest digest-pointer))

(defmacro def-digest-fun (name return-type &body arguments)
  `(defcfun (,(symbolicate "%DIGEST-" name)
             ,(format nil "librdf_digest_~a"
                      (substitute #\_ #\- (string-downcase (symbol-name name)))))
       ,return-type
     (digest digest-pointer)
     ,@arguments))

(def-digest-fun init :void)

(def-digest-fun update :void
  (buffer :pointer)
  (length size-t))

(def-digest-fun update-string :void
  (string :string))

(def-digest-fun final :void)

(def-digest-fun get-digest :pointer)

(def-digest-fun get-digest-length size-t)

(def-digest-fun to-string new-string)

(def-digest-fun print :void
  (fh :pointer))

;;; Hashes

(defcfun (%new-hash-from-string "librdf_new_hash_from_string") hash-pointer
  (world world-pointer)
  (name :string)
  (string :string))

(defcfun (%new-hash-from-array-of-strings "librdf_new_hash_from_array_of_strings") hash-pointer
  (world world-pointer)
  (name :string)
  (array :pointer))

(defcfun (%new-hash-from-hash "librdf_new_hash_from_hash") hash-pointer
  (old-hash hash-pointer))

(defcfun (%free-hash "librdf_free_hash") :void
  (hash hash-pointer))

(defcfun (%hash-get "librdf_hash_get") new-string
  (hash hash-pointer)
  (key :string))

(defcfun (%hash-get-as-boolean "librdf_hash_get_as_boolean") :int
  (hash hash-pointer)
  (key :string))

(defcfun (%hash-get-as-long "librdf_hash_get_as_long") :long
  (hash hash-pointer)
  (key :string))

(defcfun (%hash-get-del "librdf_hash_get_del") new-string
  (hash hash-pointer)
  (key :string))

(defcfun (%hash-put-strings "librdf_hash_put_strings") :int
  (hash hash-pointer)
  (key :string)
  (value :string))

(defcfun (%hash-print "librdf_hash_print") :void
  (hash hash-pointer)
  (fh :pointer))

(defcfun (%hash-print-keys "librdf_hash_print_keys") :void
  (hash hash-pointer)
  (fh :pointer))

(defcfun (%hash-print-values "librdf_hash_print_values") :void
  (hash hash-pointer)
  (key-string :string)
  (fh :pointer))

(defcfun (%hash-interpret-template "librdf_hash_interpret_template") new-string
  (template-string :string)
  (dictionary hash-pointer)
  (prefix :string)
  (suffix :string))

;;; Heuristics

(defmacro def-heuristic (name return-type &body arguments)
  `(defcfun (,(symbolicate "%HEURISTIC-" name)
              ,(format nil "librdf_heuristic_~a"
                       (substitute #\_ #\- (string-downcase (symbol-name name)))))
       ,return-type
     ,@arguments))

(def-heuristic gen-name new-string (name :string))

(def-heuristic is-blank-node :int (node :string))

(def-heuristic get-blank-node :string (node :string))

(def-heuristic object-is-literal :int (object :string))

;;; Iterator

(defmacro def-iterator (name return-type &body arguments)
  `(defcfun (,(symbolicate "%ITERATOR-" name)
              ,(format nil "librdf_iterator_~a"
                       (substitute #\_ #\- (string-downcase (symbol-name name)))))
       ,return-type
     (iterator iterator-pointer)
     ,@arguments))

(defcfun (%new-iterator "librdf_new_iterator") iterator-pointer
  (world world-pointer)
  (context :pointer)
  (is-end-methodvoid :pointer)
  (next-methodvoid :pointer)
  (get-methodvoid :pointer)
  (finished-methodvoid :pointer))

(defcfun (%free-iterator "librdf_free_iterator") :void
  (iterator iterator-pointer))

(def-iterator end :int)

(def-iterator have-elements :int)

(def-iterator next :int)

(def-iterator get-object :pointer)

(def-iterator get-context :pointer)

(def-iterator get-key :pointer)

(def-iterator get-value :pointer)

(def-iterator add-map :int
  (map-function :pointer);callback iterator,context,item->pointer
  (free-context :pointer);callback map-context->void
  (map-context :pointer))

(defcfun (%new-empty-iterator "librdf_new_empty_iterator") iterator-pointer
  (world world-pointer))

;;; List

(defcfun (%new-list "librdf_new_list") list-pointer
  (world world-pointer))

(defcfun (%free-list "librdf_free_list") :void
  (list list-pointer))

(defmacro def-list (name return-type &body arguments)
  `(defcfun (,(symbolicate "%LIST-" name)
              ,(format nil "librdf_list_~a"
                       (substitute #\_ #\- (string-downcase (symbol-name name)))))
       ,return-type
     (list list-pointer)
     ,@arguments))

(def-list clear :void)

(def-list add :int (data :pointer))

(def-list unshift :int (data :pointer))

(def-list shift :pointer)

(def-list pop :pointer)

(def-list remove :pointer (data :pointer))

(def-list contains :int (data :pointer))

(def-list size :int)

(def-list set-equals :void
  (equalsvoid :pointer));callback pointer,pointer->int

(def-list get-iterator iterator-pointer)

(def-list foreach :void
  (fnvoid :pointer);callback pointer->void
  (user-data :pointer))

;;; Logging

(defmacro def-log (name return-type &body arguments)
  `(defcfun (,(symbolicate "%LOG-MESSAGE-" name)
              ,(format nil "librdf_log_message_~a"
                       (substitute #\_ #\- (string-downcase (symbol-name name)))))
       ,return-type
     (message message-pointer)
     ,@arguments))

(def-log code :int)
(def-log level log-level)
(def-log facility log-facility)
(def-log message :string)

;;; RDF Graph
;;; semi-automatically generated, review!

(defcfun (%model-enumerate "librdf_model_enumerate") :int
  (world world-pointer)
  (counter :uint)
  (name :pointer)
  (label :pointer))

(defcfun (%new-model "librdf_new_model") model-pointer
  (world world-pointer)
  (storage storage-pointer)
  (options-string :string))

(defcfun (%new-model-with-options "librdf_new_model_with_options") model-pointer
  (world world-pointer)
  (storage storage-pointer)
  (options hash-pointer))

(defcfun (%new-model-from-model "librdf_new_model_from_model") model-pointer
  (model model-pointer))

(defcfun (%free-model "librdf_free_model") :void
  (model model-pointer))

(defcfun (%model-size "librdf_model_size") :int
  (model model-pointer))

(defcfun (%model-add "librdf_model_add") :int
  (model model-pointer)
  (subject node-pointer)
  (predicate node-pointer)
  (object node-pointer))

(defcfun (%model-add-string-literal-statement "librdf_model_add_string_literal_statement") :int
  (model model-pointer)
  (subject node-pointer)
  (predicate node-pointer)
  (literal :string)
  (xml-language :string)
  (is-wf-xml :int))

(defcfun (%model-add-typed-literal-statement "librdf_model_add_typed_literal_statement") :int
  (model model-pointer)
  (subject node-pointer)
  (predicate node-pointer)
  (literal :string)
  (xml-language :string)
  (datatype-uri uri-pointer))

(defcfun (%model-add-statement "librdf_model_add_statement") :int
  (model model-pointer)
  (statement statement-pointer))

(defcfun (%model-add-statements "librdf_model_add_statements") :int
  (model model-pointer)
  (statement-stream stream-pointer))

(defcfun (%model-remove-statement "librdf_model_remove_statement") :int
  (model model-pointer)
  (statement statement-pointer))

(defcfun (%model-contains-statement "librdf_model_contains_statement") :int
  (model model-pointer)
  (statement statement-pointer))

(defcfun (%model-has-arc-in "librdf_model_has_arc_in") :int
  (model model-pointer)
  (node node-pointer)
  (property node-pointer))

(defcfun (%model-has-arc-out "librdf_model_has_arc_out") :int
  (model model-pointer)
  (node node-pointer)
  (property node-pointer))

(defcfun (%model-as-stream "librdf_model_as_stream") stream-pointer
  (model model-pointer))

(defcfun (%model-serialise "librdf_model_serialise") stream-pointer
  (model model-pointer))

(defcfun (%model-find-statements "librdf_model_find_statements") stream-pointer
  (model model-pointer)
  (statement statement-pointer))

(defcfun (%model-find-statements-with-options "librdf_model_find_statements_with_options") stream-pointer
  (model model-pointer)
  (statement statement-pointer)
  (context-node node-pointer)
  (options hash-pointer))

(defcfun (%model-get-sources "librdf_model_get_sources") iterator-pointer
  (model model-pointer)
  (arc node-pointer)
  (target node-pointer))

(defcfun (%model-get-arcs "librdf_model_get_arcs") iterator-pointer
  (model model-pointer)
  (source node-pointer)
  (target node-pointer))

(defcfun (%model-get-targets "librdf_model_get_targets") iterator-pointer
  (model model-pointer)
  (source node-pointer)
  (arc node-pointer))

(defcfun (%model-get-source "librdf_model_get_source") node-pointer
  (model model-pointer)
  (arc node-pointer)
  (target node-pointer))

(defcfun (%model-get-arc "librdf_model_get_arc") node-pointer
  (model model-pointer)
  (source node-pointer)
  (target node-pointer))

(defcfun (%model-get-target "librdf_model_get_target") node-pointer
  (model model-pointer)
  (source node-pointer)
  (arc node-pointer))

(defcfun (%model-get-arcs-in "librdf_model_get_arcs_in") iterator-pointer
  (model model-pointer)
  (node node-pointer))

(defcfun (%model-get-arcs-out "librdf_model_get_arcs_out") iterator-pointer
  (model model-pointer)
  (node node-pointer))

(defcfun (%model-add-submodel "librdf_model_add_submodel") :int
  (model model-pointer)
  (sub-model model-pointer))

(defcfun (%model-remove-submodel "librdf_model_remove_submodel") :int
  (model model-pointer)
  (sub-model model-pointer))

(defcfun (%model-print "librdf_model_print") :void
  (model model-pointer)
  (fh :pointer))

(defcfun (%model-context-add-statement "librdf_model_context_add_statement") :int
  (model model-pointer)
  (context node-pointer)
  (statement statement-pointer))

(defcfun (%model-context-add-statements "librdf_model_context_add_statements") :int
  (model model-pointer)
  (context node-pointer)
  (stream stream-pointer))

(defcfun (%model-context-remove-statement "librdf_model_context_remove_statement") :int
  (model model-pointer)
  (context node-pointer)
  (statement statement-pointer))

(defcfun (%model-context-remove-statements "librdf_model_context_remove_statements") :int
  (model model-pointer)
  (context node-pointer))

(defcfun (%model-context-as-stream "librdf_model_context_as_stream") stream-pointer
  (model model-pointer)
  (context node-pointer))

(defcfun (%model-context-serialize "librdf_model_context_serialize") stream-pointer
  (model model-pointer)
  (context node-pointer))

(defcfun (%model-contains-context "librdf_model_contains_context") :int
  (model model-pointer)
  (context node-pointer))

(defcfun (%model-query-execute "librdf_model_query_execute") query-results-pointer
  (model model-pointer)
  (query query-pointer))

(defcfun (%model-sync "librdf_model_sync")  :int
  (model model-pointer))

(defcfun (%model-get-storage "librdf_model_get_storage") storage-pointer
  (model model-pointer))

(defcfun (%model-load "librdf_model_load")  :int
  (model model-pointer)
  (uri uri-pointer)
  (name :string)
  (mime-type :string)
  (type-uri uri-pointer))

(defcfun (%model-to-counted-string "librdf_model_to_counted_string") new-string
  (model model-pointer)
  (uri uri-pointer)
  (name :string)
  (mimeitype :string)
  (type-uri uri-pointer)
  (stringilength_p size-t))

(defcfun (%model-to-string "librdf_model_to_string") new-string
  (model model-pointer)
  (uri uri-pointer)
  (name :string)
  (mime-type :string)
  (type-uri uri-pointer))

(defcfun (%model-find-statements-in-context "librdf_model_find_statements_in_context") stream-pointer
  (model model-pointer)
  (statement statement-pointer)
  (context-node node-pointer))

(defcfun (%model-get-contexts "librdf_model_get_contexts") iterator-pointer
  (model model-pointer))

(defcfun (%model-get-feature "librdf_model_get_feature") node-pointer
  (model model-pointer)
  (feature uri-pointer))

(defcfun (%model-set-feature "librdf_model_set_feature")  :int
  (model model-pointer)
  (feature uri-pointer)
  (value node-pointer))

(defcfun (%model-transaction-commit "librdf_model_transaction_commit")  :int
  (model model-pointer))

(defcfun (%model-transaction-get-handle "librdf_model_transaction_get_handle") :pointer
  (model model-pointer))

(defcfun (%model-transaction-rollback "librdf_model_transaction_rollback")  :int
  (model model-pointer))

(defcfun (%model-transaction-start "librdf_model_transaction_start")  :int
  (model model-pointer))

(defcfun (%model-transaction-start-with-handle "librdf_model_transaction_start_with_handle")  :int
  (model model-pointer)
  (handle :pointer))

;;; RDF term
;;; generated automatically

(defcfun (%new-node "librdf_new_node") node-pointer
  (world world-pointer))

(defcfun (%new-node-from-uri-string "librdf_new_node_from_uri_string") node-pointer
  (world world-pointer)
  (uri-string :string))

(defcfun (%new-node-from-uri "librdf_new_node_from_uri") node-pointer
  (world world-pointer)
  (uri uri-pointer))

(defcfun (%new-node-from-uri-local-name "librdf_new_node_from_uri_local_name") node-pointer
  (world world-pointer)
  (uri uri-pointer)
  (local-name :string))

(defcfun (%new-node-from-normalised-uri-string "librdf_new_node_from_normalised_uri_string") node-pointer
  (world world-pointer)
  (uri-string :string)
  (source-uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%new-node-from-literal "librdf_new_node_from_literal") node-pointer
  (world world-pointer)
  (string :string)
  (xml-language :string)
  (is-wf-xml :int))

(defcfun (%new-node-from-typed-literal "librdf_new_node_from_typed_literal") node-pointer
  (world world-pointer)
  (value :string)
  (xml-language :string)
  (datatype-uri uri-pointer))

(defcfun (%new-node-from-typed-counted-literal "librdf_new_node_from_typed_counted_literal") node-pointer
  (world world-pointer)
  (value :string)
  (value-len size-t)
  (xml-language :string)
  (xml-language-len size-t)
  (datatype-uri uri-pointer))

(defcfun (%new-node-from-blank-identifier "librdf_new_node_from_blank_identifier") node-pointer
  (world world-pointer)
  (identifier :string))

(defcfun (%new-node-from-node "librdf_new_node_from_node") node-pointer
  (node node-pointer))

(defcfun (%free-node "librdf_free_node") :void
  (node node-pointer))

(defcfun (%node-get-uri "librdf_node_get_uri") uri-pointer
  (node node-pointer))

(defcfun (%node-get-type "librdf_node_get_type") node-type
  (node node-pointer))

(defcfun (%node-get-literal-value "librdf_node_get_literal_value") :string
  (node node-pointer))

(defcfun (%node-get-literal-value-as-counted-string "librdf_node_get_literal_value_as_counted_string") :string
  (node node-pointer)
  (len-p :pointer))

(defcfun (%node-get-literal-value-as-latin1 "librdf_node_get_literal_value_as_latin1") new-string
  (node node-pointer))

(defcfun (%node-get-literal-value-language "librdf_node_get_literal_value_language") new-string
  (node node-pointer))

(defcfun (%node-get-literal-value-is-wf-xml "librdf_node_get_literal_value_is_wf_xml") :int
  (node node-pointer))

(defcfun (%node-get-literal-value-datatype-uri "librdf_node_get_literal_value_datatype_uri") uri-pointer
  (node node-pointer))

(defcfun (%node-get-li-ordinal "librdf_node_get_li_ordinal") :int
  (node node-pointer))

(defcfun (%node-get-blank-identifier "librdf_node_get_blank_identifier") :string
  (node node-pointer))

(defcfun (%node-is-resource "librdf_node_is_resource") :int
  (node node-pointer))

(defcfun (%node-is-literal "librdf_node_is_literal") :int
  (node node-pointer))

(defcfun (%node-is-blank "librdf_node_is_blank") :int
  (node node-pointer))

(defcfun (%node-encode "librdf_node_encode") size-t
  (node node-pointer)
  (buffer :pointer)
  (length size-t))

(defcfun (%node-decode "librdf_node_decode") node-pointer
  (world world-pointer)
  (size-p :pointer)
  (buffer :pointer)
  (length size-t))

(defcfun (%node-to-string "librdf_node_to_string") :string
  (node node-pointer))

(defcfun (%node-to-counted-string "librdf_node_to_counted_string") :string
  (node node-pointer)
  (len-p :pointer))

(defcfun (%node-print "librdf_node_print") :void
  (node node-pointer)
  (fh :pointer))

(defcfun (%node-equals "librdf_node_equals") :int
  (first-node node-pointer)
  (second-node node-pointer))

(defcfun (%node-static-iterator-create "librdf_node_static_iterator_create") iterator-pointer
  (nodes :pointer)
  (size :int))

;;; Parser
;;; automatically generated, some with callbacks missing

(defcfun (%parser-enumerate "librdf_parser_enumerate") :int
  (world world-pointer)
  (counter :uint)
  (name :pointer)
  (label :pointer))

(defcfun (%parser-guess-name "librdf_parser_guess_name") :string
  (world world-pointer)
  (mime-type :string)
  (buffer :pointer)
  (identifier :pointer))

(defcfun (%new-parser "librdf_new_parser") parser-pointer
  (world world-pointer)
  (name :string)
  (mime-type :string)
  (type-uri uri-pointer))

(defcfun (%new-parser-from-factory "librdf_new_parser_from_factory") parser-pointer
  (world world-pointer)
  (factory parser-factory-pointer))

(defcfun (%free-parser "librdf_free_parser") :void
  (parser parser-pointer))

(defcfun (%parser-parse-as-stream "librdf_parser_parse_as_stream") stream-pointer
  (parser parser-pointer)
  (uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%parser-parse-into-model "librdf_parser_parse_into_model") :int
  (parser parser-pointer)
  (uri uri-pointer)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%parser-parse-file-handle-as-stream "librdf_parser_parse_file_handle_as_stream") stream-pointer
  (parser parser-pointer)
  (fh :pointer)
  (close-fh :int)
  (base-uri uri-pointer))

(defcfun (%parser-parse-file-handle-into-model "librdf_parser_parse_file_handle_into_model") :int
  (parser parser-pointer)
  (fh :pointer)
  (close-fh :int)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%parser-parse-string-as-stream "librdf_parser_parse_string_as_stream") stream-pointer
  (parser parser-pointer)
  (string :pointer)
  (base-uri uri-pointer))

(defcfun (%parser-parse-string-into-model "librdf_parser_parse_string_into_model") :int
  (parser parser-pointer)
  (string :pointer)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%parser-parse-counted-string-as-stream "librdf_parser_parse_counted_string_as_stream") stream-pointer
  (parser parser-pointer)
  (string :pointer)
  (length size-t)
  (base-uri uri-pointer))

(defcfun (%parser-parse-counted-string-into-model "librdf_parser_parse_counted_string_into_model") :int
  (parser parser-pointer)
  (string :pointer)
  (length size-t)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%parser-get-feature "librdf_parser_get_feature") node-pointer
  (parser parser-pointer)
  (feature uri-pointer))

(defcfun (%parser-set-feature "librdf_parser_set_feature") :int
  (parser parser-pointer)
  (feature uri-pointer)
  (value node-pointer))

(defcfun (%parser-get-accept-header "librdf_parser_get_accept_header") new-string
  (parser parser-pointer))

(defcfun (%parser-get-namespaces-seen-count "librdf_parser_get_namespaces_seen_count") :int
  (parser parser-pointer))

(defcfun (%parser-get-namespaces-seen-prefix "librdf_parser_get_namespaces_seen_prefix") :string
  (parser parser-pointer)
  (offset :int))

(defcfun (%parser-get-namespaces-seen-uri "librdf_parser_get_namespaces_seen_uri") uri-pointer
  (parser parser-pointer)
  (offset :int))

(defcfun (%parser-get-uri-filter "librdf_parser_get_uri_filter") :pointer
  (parser parser-pointer)
  (user-data-p :pointer))

(defcfun (%parser-set-uri-filter "librdf_parser_set_uri_filter") :void
  (parser parser-pointer)
  (librdf-uri-filter-funcfilter :pointer)
  (user-data :pointer))

;;; Query
;;; automatically generated

(defcfun (%new-query "librdf_new_query") query-pointer
  (world world-pointer)
  (name :string)
  (uri uri-pointer)
  (query-string :pointer)
  (base-uri uri-pointer))

(defcfun (%new-query-from-query "librdf_new_query_from_query") query-pointer
  (old-query query-pointer))

(defcfun (%new-query-from-factory "librdf_new_query_from_factory") query-pointer
  (world world-pointer)
  (factory query-factory-pointer)
  (name :string)
  (uri uri-pointer)
  (query-string :pointer)
  (base-uri uri-pointer))

(defcfun (%free-query "librdf_free_query") :void
  (query query-pointer))

(defcfun (%query-execute "librdf_query_execute") query-results-pointer
  (query query-pointer)
  (model model-pointer))

(defcfun (%query-get-limit "librdf_query_get_limit") :int
  (query query-pointer))

(defcfun (%query-set-limit "librdf_query_set_limit") :int
  (query query-pointer)
  (limit :int))

(defcfun (%query-get-offset "librdf_query_get_offset") :int
  (query query-pointer))

(defcfun (%query-set-offset "librdf_query_set_offset") :int
  (query query-pointer)
  (offset :int))

;;; Query results
;;; automatically generated

(defcfun (%query-results-as-stream "librdf_query_results_as_stream") stream-pointer
  (query-results query-results-pointer))

(defcfun (%query-results-get-count "librdf_query_results_get_count") :int
  (query-results query-results-pointer))

(defcfun (%query-results-next "librdf_query_results_next") :int
  (query-results query-results-pointer))

(defcfun (%query-results-finished "librdf_query_results_finished") :int
  (query-results query-results-pointer))

(defcfun (%query-results-get-bindings "librdf_query_results_get_bindings") :int
  (query-results query-results-pointer)
  (names :pointer)
  (values :pointer))

(defcfun (%query-results-get-binding-value "librdf_query_results_get_binding_value") node-pointer
  (query-results query-results-pointer)
  (offset :int))

(defcfun (%query-results-get-binding-name "librdf_query_results_get_binding_name") :string
  (query-results query-results-pointer)
  (offset :int))

(defcfun (%query-results-get-binding-value-by-name "librdf_query_results_get_binding_value_by_name") node-pointer
  (query-results query-results-pointer)
  (name :string))

(defcfun (%query-results-get-bindings-count "librdf_query_results_get_bindings_count") :int
  (query-results query-results-pointer))

(defcfun (%query-results-to-counted-string "librdf_query_results_to_counted_string") :string
  (query-results query-results-pointer)
  (format-uri uri-pointer)
  (base-uri uri-pointer)
  (length-p :pointer))

(defcfun (%query-results-to-string "librdf_query_results_to_string") :string
  (query-results query-results-pointer)
  (format-uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%query-results-to-file-handle "librdf_query_results_to_file_handle") :int
  (query-results query-results-pointer)
  (handle :pointer)
  (format-uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%query-results-to-file "librdf_query_results_to_file") :int
  (query-results query-results-pointer)
  (name :string)
  (format-uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%free-query-results "librdf_free_query_results") :void
  (query-results query-results-pointer))

(defcfun (%query-results-is-bindings "librdf_query_results_is_bindings") :int
  (query-results query-results-pointer))

(defcfun (%query-results-is-boolean "librdf_query_results_is_boolean") :int
  (query-results query-results-pointer))

(defcfun (%query-results-is-graph "librdf_query_results_is_graph") :int
  (query-results query-results-pointer))

(defcfun (%query-results-is-syntax "librdf_query_results_is_syntax") :int
  (query-results query-results-pointer))

(defcfun (%query-results-get-boolean "librdf_query_results_get_boolean") :int
  (query-results query-results-pointer))

(defcfun (%new-query-results-formatter "librdf_new_query_results_formatter") query-results-formatter-pointer
  (query-results query-results-pointer)
  (name :string)
  (uri uri-pointer))

(defcfun (%new-query-results-formatter-by-mime-type "librdf_new_query_results_formatter_by_mime_type") query-results-formatter-pointer
  (query-results query-results-pointer)
  (mime-type :string))

(defcfun (%free-query-results-formatter "librdf_free_query_results_formatter") :void
  (formatter query-results-formatter-pointer))

(defcfun (%query-results-formats-check "librdf_query_results_formats_check") :int
  (world world-pointer)
  (name :string)
  (uri uri-pointer)
  (mime-type :string))

(defcfun (%query-results-formats-enumerate "librdf_query_results_formats_enumerate") :int
  (world world-pointer)
  (counter :uint)
  (name :pointer)
  (label :pointer)
  (uri-string :pointer)
  (mime-type :pointer))

(defcfun (%query-results-formatter-write "librdf_query_results_formatter_write") :int
  (iostr raptor-iostream-pointer)
  (formatter query-results-formatter-pointer)
  (results query-results-pointer)
  (base-uri uri-pointer))

;;; Serializers
;;; automatically generated

(defcfun (%serializer-enumerate "librdf_serializer_enumerate") :int
  (world world-pointer)
  (counter :uint)
  (name :pointer)
  (label :pointer))

(defcfun (%new-serializer "librdf_new_serializer") serializer-pointer
  (world world-pointer)
  (name :string)
  (mime-type :string)
  (type-uri uri-pointer))

(defcfun (%new-serializer-from-factory "librdf_new_serializer_from_factory") serializer-pointer
  (world world-pointer)
  (factory serializer-factory-pointer))

(defcfun (%free-serializer "librdf_free_serializer") :void
  (serializer serializer-pointer))

(defcfun (%serializer-serialize-model "librdf_serializer_serialize_model") :int
  (serializer serializer-pointer)
  (handle :pointer)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%serializer-serialize-model-to-file-handle "librdf_serializer_serialize_model_to_file_handle") :int
  (serializer serializer-pointer)
  (handle :pointer)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%serializer-serialize-model-to-file "librdf_serializer_serialize_model_to_file") :int
  (serializer serializer-pointer)
  (name :string)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%serializer-serialize-model-to-string "librdf_serializer_serialize_model_to_string") :string
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (model model-pointer))

(defcfun (%serializer-serialize-model-to-counted-string "librdf_serializer_serialize_model_to_counted_string") :string
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (model model-pointer)
  (length-p :pointer))

(defcfun (%serializer-serialize-model-to-iostream "librdf_serializer_serialize_model_to_iostream") :int
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (model model-pointer)
  (iostr raptor-iostream-pointer))

(defcfun (%serializer-serialize-stream-to-counted-string "librdf_serializer_serialize_stream_to_counted_string") :string
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (stream stream-pointer)
  (length-p :pointer))

(defcfun (%serializer-serialize-stream-to-file "librdf_serializer_serialize_stream_to_file") :int
  (serializer serializer-pointer)
  (name :string)
  (base-uri uri-pointer)
  (stream stream-pointer))

(defcfun (%serializer-serialize-stream-to-file-handle "librdf_serializer_serialize_stream_to_file_handle") :int
  (serializer serializer-pointer)
  (handle :pointer)
  (base-uri uri-pointer)
  (stream stream-pointer))

(defcfun (%serializer-serialize-stream-to-iostream "librdf_serializer_serialize_stream_to_iostream") :int
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (stream stream-pointer)
  (iostr raptor-iostream-pointer))

(defcfun (%serializer-serialize-stream-to-string "librdf_serializer_serialize_stream_to_string") :string
  (serializer serializer-pointer)
  (base-uri uri-pointer)
  (stream stream-pointer))

(defcfun (%serializer-get-feature "librdf_serializer_get_feature") node-pointer
  (serializer serializer-pointer)
  (feature uri-pointer))

(defcfun (%serializer-set-feature "librdf_serializer_set_feature") :int
  (serializer serializer-pointer)
  (feature uri-pointer)
  (value node-pointer))

(defcfun (%serializer-set-namespace "librdf_serializer_set_namespace") :int
  (serializer serializer-pointer)
  (uri uri-pointer)
  (prefix :string))

;;; RDF Triple
;;; automatically generated

(defcfun (%new-statement "librdf_new_statement") statement-pointer
  (world world-pointer))

(defcfun (%new-statement-from-statement "librdf_new_statement_from_statement") statement-pointer
  (statement statement-pointer))

(defcfun (%new-statement-from-nodes "librdf_new_statement_from_nodes") statement-pointer
  (world world-pointer)
  (subject node-pointer)
  (predicate node-pointer)
  (object node-pointer))

(defcfun (%statement-init "librdf_statement_init") :void
  (world world-pointer)
  (statement statement-pointer))

(defcfun (%statement-clear "librdf_statement_clear") :void
  (statement statement-pointer))

(defcfun (%free-statement "librdf_free_statement") :void
  (statement statement-pointer))

(defcfun (%statement-get-subject "librdf_statement_get_subject") node-pointer
  (statement statement-pointer))

(defcfun (%statement-set-subject "librdf_statement_set_subject") :void
  (statement statement-pointer)
  (node node-pointer))

(defcfun (%statement-get-predicate "librdf_statement_get_predicate") node-pointer
  (statement statement-pointer))

(defcfun (%statement-set-predicate "librdf_statement_set_predicate") :void
  (statement statement-pointer)
  (node node-pointer))

(defcfun (%statement-get-object "librdf_statement_get_object") node-pointer
  (statement statement-pointer))

(defcfun (%statement-set-object "librdf_statement_set_object") :void
  (statement statement-pointer)
  (node node-pointer))

(defcfun (%statement-is-complete "librdf_statement_is_complete") :int
  (statement statement-pointer))

(defcfun (%statement-to-string "librdf_statement_to_string") :string
  (statement statement-pointer))

(defcfun (%statement-print "librdf_statement_print") :void
  (statement statement-pointer)
  (fh :pointer))

(defcfun (%statement-equals "librdf_statement_equals") :int
  (statement1 statement-pointer)
  (statement2 statement-pointer))

(defcfun (%statement-match "librdf_statement_match") :int
  (statement statement-pointer)
  (partial-statement statement-pointer))

(defcfun (%statement-encode "librdf_statement_encode") size-t
  (statement statement-pointer)
  (buffer :pointer)
  (length size-t))

(defcfun (%statement-encode-parts "librdf_statement_encode_parts") size-t
  (statement statement-pointer)
  (context-node node-pointer)
  (buffer :pointer)
  (length size-t)
  (fields statement-part))

(defcfun (%statement-decode "librdf_statement_decode") size-t
  (statement statement-pointer)
  (buffer :pointer)
  (length size-t))

(defcfun (%statement-decode-parts "librdf_statement_decode_parts") size-t
  (statement statement-pointer)
  (context-node :pointer)
  (buffer :pointer)
  (length size-t))

;;; Storage
;;; automatically generated

(defcfun (%storage-enumerate "librdf_storage_enumerate") :int
  (world world-pointer)
  (counter :uint)
  (name :pointer)
  (label :pointer))

(defcfun (%new-storage "librdf_new_storage") storage-pointer
  (world world-pointer)
  (storage-name :string)
  (name :string)
  (options-string :string))

(defcfun (%new-storage-with-options "librdf_new_storage_with_options") storage-pointer
  (world world-pointer)
  (storage-name :string)
  (name :string)
  (options hash-pointer))

(defcfun (%new-storage-from-storage "librdf_new_storage_from_storage") storage-pointer
  (old-storage storage-pointer))

(defcfun (%new-storage-from-factory "librdf_new_storage_from_factory") storage-pointer
  (world world-pointer)
  (factory storage-factory-pointer)
  (name :string)
  (options hash-pointer))

(defcfun (%free-storage "librdf_free_storage") :void
  (storage storage-pointer))

(defcfun (%storage-open "librdf_storage_open") :int
  (storage storage-pointer)
  (model model-pointer))

(defcfun (%storage-close "librdf_storage_close") :int
  (storage storage-pointer))

(defcfun (%storage-size "librdf_storage_size") :int
  (storage storage-pointer))

(defcfun (%storage-add-statement "librdf_storage_add_statement") :int
  (storage storage-pointer)
  (statement statement-pointer))

(defcfun (%storage-add-statements "librdf_storage_add_statements") :int
  (storage storage-pointer)
  (statement-stream stream-pointer))

(defcfun (%storage-remove-statement "librdf_storage_remove_statement") :int
  (storage storage-pointer)
  (statement statement-pointer))

(defcfun (%storage-contains-statement "librdf_storage_contains_statement") :int
  (storage storage-pointer)
  (statement statement-pointer))

(defcfun (%storage-serialise "librdf_storage_serialise") stream-pointer
  (storage storage-pointer))

(defcfun (%storage-find-statements "librdf_storage_find_statements") stream-pointer
  (storage storage-pointer)
  (statement statement-pointer))

(defcfun (%storage-find-statements-with-options "librdf_storage_find_statements_with_options") stream-pointer
  (storage storage-pointer)
  (statement statement-pointer)
  (context-node node-pointer)
  (options hash-pointer))

(defcfun (%storage-get-sources "librdf_storage_get_sources") iterator-pointer
  (storage storage-pointer)
  (arc node-pointer)
  (target node-pointer))

(defcfun (%storage-get-arcs "librdf_storage_get_arcs") iterator-pointer
  (storage storage-pointer)
  (source node-pointer)
  (target node-pointer))

(defcfun (%storage-get-targets "librdf_storage_get_targets") iterator-pointer
  (storage storage-pointer)
  (source node-pointer)
  (arc node-pointer))

(defcfun (%storage-get-arcs-in "librdf_storage_get_arcs_in") iterator-pointer
  (storage storage-pointer)
  (node node-pointer))

(defcfun (%storage-get-arcs-out "librdf_storage_get_arcs_out") iterator-pointer
  (storage storage-pointer)
  (node node-pointer))

(defcfun (%storage-has-arc-in "librdf_storage_has_arc_in") :int
  (storage storage-pointer)
  (node node-pointer)
  (property node-pointer))

(defcfun (%storage-has-arc-out "librdf_storage_has_arc_out") :int
  (storage storage-pointer)
  (node node-pointer)
  (property node-pointer))

(defcfun (%storage-context-add-statement "librdf_storage_context_add_statement") :int
  (storage storage-pointer)
  (context node-pointer)
  (statement statement-pointer))

(defcfun (%storage-context-add-statements "librdf_storage_context_add_statements") :int
  (storage storage-pointer)
  (context node-pointer)
  (stream stream-pointer))

(defcfun (%storage-context-remove-statement "librdf_storage_context_remove_statement") :int
  (storage storage-pointer)
  (context node-pointer)
  (statement statement-pointer))

(defcfun (%storage-context-remove-statements "librdf_storage_context_remove_statements") :int
  (storage storage-pointer)
  (context node-pointer))

(defcfun (%storage-context-as-stream "librdf_storage_context_as_stream") stream-pointer
  (storage storage-pointer)
  (context node-pointer))

(defcfun (%storage-context-serialise "librdf_storage_context_serialise") stream-pointer
  (storage storage-pointer)
  (context node-pointer))

(defcfun (%storage-supports-query "librdf_storage_supports_query") :int
  (storage storage-pointer)
  (query query-pointer))

(defcfun (%storage-query-execute "librdf_storage_query_execute") query-results-pointer
  (storage storage-pointer)
  (query query-pointer))

(defcfun (%storage-sync "librdf_storage_sync") :int
  (storage storage-pointer))

(defcfun (%storage-find-statements-in-context "librdf_storage_find_statements_in_context") stream-pointer
  (storage storage-pointer)
  (statement statement-pointer)
  (context-node node-pointer))

(defcfun (%storage-get-contexts "librdf_storage_get_contexts") iterator-pointer
  (storage storage-pointer))

(defcfun (%storage-get-feature "librdf_storage_get_feature") node-pointer
  (storage storage-pointer)
  (feature uri-pointer))

(defcfun (%storage-set-feature "librdf_storage_set_feature") :int
  (storage storage-pointer)
  (feature uri-pointer)
  (value node-pointer))

(defcfun (%storage-transaction-commit "librdf_storage_transaction_commit") :int
  (storage storage-pointer))

(defcfun (%storage-transaction-get-handle "librdf_storage_transaction_get_handle") :pointer
  (storage storage-pointer))

(defcfun (%storage-transaction-rollback "librdf_storage_transaction_rollback") :int
  (storage storage-pointer))

(defcfun (%storage-transaction-start "librdf_storage_transaction_start") :int
  (storage storage-pointer))

(defcfun (%storage-transaction-start-with-handle "librdf_storage_transaction_start_with_handle") :int
  (storage storage-pointer)
  (handle :pointer))

(defcfun (%storage-add-reference "librdf_storage_add_reference") :void
  (storage storage-pointer))

(defcfun (%storage-remove-reference "librdf_storage_remove_reference") :void
  (storage storage-pointer))

;;; Stream of triples
;;; automatically generated, some callback-using functions missing

(defcfun (%new-stream-from-node-iterator "librdf_new_stream_from_node_iterator") stream-pointer
  (iterator iterator-pointer)
  (statement statement-pointer)
  (field statement-part))

(defcfun (%new-empty-stream "librdf_new_empty_stream") stream-pointer
  (world world-pointer))

(defcfun (%free-stream "librdf_free_stream") :void
  (stream stream-pointer))

(defcfun (%stream-end "librdf_stream_end") :int
  (stream stream-pointer))

(defcfun (%stream-next "librdf_stream_next") :int
  (stream stream-pointer))

(defcfun (%stream-get-object "librdf_stream_get_object") statement-pointer
  (stream stream-pointer))

(defcfun (%stream-get-context "librdf_stream_get_context") :pointer
  (stream stream-pointer))

(defcfun (%stream-add-map "librdf_stream_add_map") :int
  (stream stream-pointer)
  (map-function :pointer)
  (free-context :pointer)
  (map-context :pointer))

(defcfun (%stream-print "librdf_stream_print") :void
  (stream stream-pointer)
  (fh :pointer))

;;; URI
;;; automatically generated

(defcfun (%new-uri "librdf_new_uri") uri-pointer
  (world world-pointer)
  (uri-string :string))

(defcfun (%new-uri-from-uri "librdf_new_uri_from_uri") uri-pointer
  (old-uri uri-pointer))

(defcfun (%new-uri-from-uri-local-name "librdf_new_uri_from_uri_local_name") uri-pointer
  (old-uri uri-pointer)
  (local-name :string))

(defcfun (%free-uri "librdf_free_uri") :void
  (uri uri-pointer))

(defcfun (%uri-as-string "librdf_uri_as_string") :string
  (uri uri-pointer))

(defcfun (%uri-as-counted-string "librdf_uri_as_counted_string") :string
  (uri uri-pointer)
  (len-p :pointer))

(defcfun (%uri-print "librdf_uri_print") :void
  (uri uri-pointer)
  (fh :pointer))

(defcfun (%uri-to-string "librdf_uri_to_string") new-string
  (uri uri-pointer))

(defcfun (%uri-to-counted-string "librdf_uri_to_counted_string") :string
  (uri uri-pointer)
  (len-p :pointer))

(defcfun (%uri-equals "librdf_uri_equals") :int
  (first-uri uri-pointer)
  (second-uri uri-pointer))

(defcfun (%uri-is-file-uri "librdf_uri_is_file_uri") :int
  (uri uri-pointer))

(defcfun (%uri-to-filename "librdf_uri_to_filename") :string
  (uri uri-pointer))

(defcfun (%new-uri-normalised-to-base "librdf_new_uri_normalised_to_base") uri-pointer
  (uri-string :string)
  (source-uri uri-pointer)
  (base-uri uri-pointer))

(defcfun (%new-uri-relative-to-base "librdf_new_uri_relative_to_base") uri-pointer
  (base-uri uri-pointer)
  (uri-string :string))

(defcfun (%new-uri-from-filename "librdf_new_uri_from_filename") uri-pointer
  (world world-pointer)
  (filename :string))

(defcfun (%uri-compare "librdf_uri_compare") :int
  (uri1 uri-pointer)
  (uri2 uri-pointer))
