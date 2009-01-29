(in-package :redland)

(flag "-I/usr/include/rasqal")

(include "redland.h")

;;(constant (+world-feature-genid-base+ "LIBRDF_WORLD_FEATURE_GENID_BASE"))
;;(constant (+world-feature-genid-counter+ "LIBRDF_WORLD_FEATURE_GENID_COUNTER"))


(ctype size-t "size_t")

(cenum concepts-index
       ((:ms-alt  "LIBRDF_CONCEPT_MS_Alt"))
       ((:ms-bag  "LIBRDF_CONCEPT_MS_Bag"))
       ((:ms-property  "LIBRDF_CONCEPT_MS_Property"))
       ((:ms-seq  "LIBRDF_CONCEPT_MS_Seq"))
       ((:ms-statement  "LIBRDF_CONCEPT_MS_Statement"))
       ((:ms-object  "LIBRDF_CONCEPT_MS_object"))
       ((:ms-predicate  "LIBRDF_CONCEPT_MS_predicate"))
       ((:ms-subject  "LIBRDF_CONCEPT_MS_subject"))
       ((:ms-type  "LIBRDF_CONCEPT_MS_type"))
       ((:ms-value  "LIBRDF_CONCEPT_MS_value"))
       ((:ms-li  "LIBRDF_CONCEPT_MS_li"))
       ((:ms-rdf  "LIBRDF_CONCEPT_MS_RDF"))
       ((:ms-description  "LIBRDF_CONCEPT_MS_Description"))
       ((:ms-about-each  "LIBRDF_CONCEPT_MS_aboutEach"))
       ((:ms-about-each-prefix  "LIBRDF_CONCEPT_MS_aboutEachPrefix"))
       ((:rs-node-id  "LIBRDF_CONCEPT_RS_nodeID"))
       ((:rs-list  "LIBRDF_CONCEPT_RS_List"))
       ((:rs-first  "LIBRDF_CONCEPT_RS_first"))
       ((:rs-rest  "LIBRDF_CONCEPT_RS_rest"))
       ((:rs-nil  "LIBRDF_CONCEPT_RS_nil"))
       ((:rs-xml-literal  "LIBRDF_CONCEPT_RS_XMLLiteral"))
       ((:s-class  "LIBRDF_CONCEPT_S_Class"))
       ((:s-constraint-property  "LIBRDF_CONCEPT_S_ConstraintProperty"))
       ((:s-constraint-resource  "LIBRDF_CONCEPT_S_ConstraintResource"))
       ((:s-container  "LIBRDF_CONCEPT_S_Container"))
       ((:s-container-membership-property  "LIBRDF_CONCEPT_S_ContainerMembershipProperty"))
       ((:s-literal  "LIBRDF_CONCEPT_S_Literal"))
       ((:s-resource  "LIBRDF_CONCEPT_S_Resource"))
       ((:s-comment  "LIBRDF_CONCEPT_S_comment"))
       ((:s-domain  "LIBRDF_CONCEPT_S_domain"))
       ((:s-is-defined-by  "LIBRDF_CONCEPT_S_isDefinedBy"))
       ((:s-label  "LIBRDF_CONCEPT_S_label"))
       ((:s-range  "LIBRDF_CONCEPT_S_range"))
       ((:s-see-also  "LIBRDF_CONCEPT_S_seeAlso"))
       ((:s-subclass-of  "LIBRDF_CONCEPT_S_subClassOf"))
       ((:s-subproperty-of  "LIBRDF_CONCEPT_S_subPropertyOf"))
       ((:first-s-id  "LIBRDF_CONCEPT_FIRST_S_ID"))
       ((:last  "LIBRDF_CONCEPT_LAST")))

(cenum node-type
       ((:unknown  "LIBRDF_NODE_TYPE_UNKNOWN"))
       ((:resource  "LIBRDF_NODE_TYPE_RESOURCE"))
       ((:literal  "LIBRDF_NODE_TYPE_LITERAL"))
       ((:blank  "LIBRDF_NODE_TYPE_BLANK"))
       ((:last  "LIBRDF_NODE_TYPE_LAST")))

(cenum statement-part
       ((:subject "LIBRDF_STATEMENT_SUBJECT"))
       ((:predicate "LIBRDF_STATEMENT_PREDICATE"))
       ((:object "LIBRDF_STATEMENT_OBJECT"))
       ((:all "LIBRDF_STATEMENT_ALL")))