;;;; joysen.lisp

(in-package #:joysen)

;;;; Joysen JSON encoding library
;;;;
;;;; A mechanism for convenient encoding (nested) lisp objects into
;;;; strings in JSON syntax.
;;;;
;;;; This operates in two modes: Normal or Implicit mode.
;;;;
;;;; Normal (explicit) mode:
;;;;
;;;;   Encoding happens by the (lisp) value and a schema as two
;;;;   separate entities, as provided to JOYSEN:ENCODE. The value is
;;;;   an arbitrary lisp object, and the schema (a list tree
;;;;   structure) describes how that value is mapped to JSON
;;;;   syntax. The lisp value and the schema are traversed in
;;;;   parallell while generating JSON output.
;;;;
;;;;   The schema object is a (lisp list) tree structure where the
;;;;   nodes name JSON encoder functions that map a lisp value to a
;;;;   (JSON) string. Each such function takes (implicitly) the lisp
;;;;   value as its first argument, and optionally more parameters. In
;;;;   the schema, an encoder function is named either by its symbol
;;;;   (which is applied to a single argument: the lisp value) or it
;;;;   is named by a list (<symbol> <args*>), in which case the symbol
;;;;   is applied to the lisp value and the provided ARGS. The ARGS
;;;;   can hold further sub-schemas, thus creating the tree structure.
;;;;
;;;;   The Joysen library provides encoder functions for typical
;;;;   mappings, e.g. integers (joysen:json-integer), plists to
;;;;   'objects' (joysen:json-object), lists or vectors to arrays
;;;;   (joysen:json-array), etc. However you can easily provide and
;;;;   use your own encoder functions that map your objects either to
;;;;   calls to Joysen encoder functions or directly to (JSON)
;;;;   strings.
;;;;
;;;;   Normal mode is useful for encoding values used otherwise in
;;;;   lisp code, and to give those a JSON representation.
;;;;
;;;;   Usage: (JOYSEN:ENCODE <value> <schema>)
;;;;   Example: (encode (list :foo-bar 42)
;;;;                          '(json-object :foo-bar json-integer)
;;;;                    :keyword :camel)
;;;;
;;;; Implicit mode:
;;;;
;;;;   A lisp form is used to construct JSON output directly. There is
;;;;   no explicit 'schema' object, rather the JSON mapping
;;;;   declarations are intertwined with the lisp forms that generate
;;;;   the values. That is, the JSON encoder functions are called as
;;;;   you normally would. The with-implicit-json macro will then
;;;;   prettify the resulting output.
;;;;
;;;;   Implicit mode is useful for generating one-off JSON syntax that
;;;;   doesn't correspond to any particular lisp objects/values.
;;;;
;;;;   Usage: (JOYSEN:WITH-IMPLICIT-JSON (<options>) <form>)
;;;;   Example: (with-implicit-json (:keyword :camel)
;;;;              (json-object (list :foo-bar (json-integer 42))))
;;;;
;;;; Each example encodes a JSON 'object' with one slot 'fooBar' that
;;;; is to be encoded as an integer that happens to be 42.
;;;;

(defvar *json-encode-trace* nil
  "Trace stack for nested JSON formatting.")

(defvar *json-implicit-mode* nil
  "False unless JSON-encoding is in 'implicit mode'.")

(defvar *json-keyword-function* nil
  "The function used to map indexes to JSON names when
  *JSON-KEYWORD-MODE* is NIL.")

(defvar *json-keyword-mode* :camel
  "How to encode keywords. NIL means to call
  *JSON-KEYWORD-FUNCTION*.")

(defvar *json-indent* 0)

(defvar *json-quote* #\"
  "The character used to quote JSON strings. Only useful for testing
purposes when the standard double-quote makes the lisp output
unreadable.")

(defvar *json-space* t
  "The string used as space between index and value. T denotes a single
space, NIL denotes no space.")

(defvar *json-null-schema*
  '((string json-string)
    (integer json-integer)
    (real json-decimal))
  "In explicit mode: The schema ([(type schema)]*) to use by
JOYSEN:ENCODE when a NIL schema is provided.

In implicit mode: Any non-string value encountered is encoded by the
value's type as specified here.")

(defparameter %json-implicit-indent-marker% (code-char #x4242)
  "Internal marker character used for encoding indentation in implicit
mode. Any character that won't otherwise exist in the output.")

(defun json-encode-keyword (name)
  "Encode NAME as a JSON keyword."
  (ecase *json-keyword-mode*
    (:camel
     (str:camel-case name))
    (:upcase
     (string-upcase name))
    (:downcase
     (string-downcase name))
    (:keep
     name)
    ((nil)
     (funcall (or *json-keyword-function*
		  (error "No ~S and no ~S specified." '*json-keyword-mode* '*json-keyword-function*))
	      name))))

(defun json-encode-space ()
  (etypecase *json-space*
    (null "")
    ((eql t) " ")
    (string *json-space*)))

(defmacro with-implicit-json ((&key (keyword '*json-keyword-mode*)
				 testp
				 (space '*json-space*)
				 (quote-char `(if ,testp #\' #\"))
				 (indent 0))
			      &body json-forms)
  "Run JSON-FORMS in 'implicit mode' JSON encoding."
  `(json-indent-implicit-encoding
    (let ((*json-implicit-mode* 0)
	  (*json-keyword-mode* ,keyword)
	  (*json-quote* ,quote-char)
	  (*json-space* ,space)
	  (*json-indent* ,indent))
      ,@json-forms)))

(defmacro with-json-index-trace ((index) &body body)
  `(let ((*json-encode-trace* (cons ,index *json-encode-trace*)))
     ,@body))

(define-condition json-unknown-value (error)
  ((value
    :initarg :value
    :reader json-unknown-value)))

(defmethod print-object ((object json-unknown-value) stream)
  (if *json-implicit-mode*
      (format stream "No implicit JSON encoding of value ~S."
	      (json-unknown-value object))
      (format stream "Unable to encode value ~S in current schema~@[: ~A~]"
	      (json-unknown-value object)
	      (format-json-trace)))
  object)

(defun json-encode-error (string &rest arguments)
  (apply #'error
	 (format nil "~A~@[: ~A~]" string (format-json-trace))
	 arguments))

(defmacro #0=json-encode-assert (test-form &optional places)
  `(assert ,test-form ,places
	   "~S ~S failed~@[: ~A~]"
	   '#0#
	   ',test-form
	   (format-json-trace)))

(defun json-encode-newline (stream &optional colon-p &rest args)
  (declare (ignore args))
  (cond
    (*json-indent*
     (terpri stream))
    (colon-p
     (write-string (json-encode-space) stream)))
  (cond
    ((not *json-indent*))
    (*json-implicit-mode*
     (write-char #\tab stream))
    ((not *json-implicit-mode*)
     (loop repeat (json-current-indent) do (write-char #\space stream))))
  (values))

(defun json-encode-prefix (prefix stream)
  (cond
    ((or (not *json-indent*)
	 (not *json-implicit-mode*))
      (write-string prefix stream))
    (t (format stream "~C~D ~A"
	       %json-implicit-indent-marker%
	       (incf *json-implicit-mode*)
	       prefix))))

(defun json-encode-postfix (postfix stream)
  (cond
    ((or (not *json-indent*)
	 (not *json-implicit-mode*))
     (write-string postfix stream))
    (t (format stream "~C~D ~A"
	       %json-implicit-indent-marker%
	       *json-implicit-mode*
	       postfix))))

(defun json-next-indent ()
  (etypecase *json-indent*
    (null nil)
    ((eql t) 4)
    ((integer 0)
     (+ *json-indent* 4))))

(defun json-current-indent ()
  (etypecase *json-indent*
    (null 0)
    ((eql t) 0)
    ((integer 0)
     *json-indent*)))

(defmacro with-json-encode-indentation ((stream-var object prefix postfix &key) &body body)
  `(with-output-to-string (,stream-var)
     (json-encode-prefix ,prefix ,stream-var)
     (prog1 (let ((*json-indent* (json-next-indent))
		  (*json-encode-trace* (cons ,object *json-encode-trace*)))
	      (json-encode-newline ,stream-var)
	      ,@body)
       (json-encode-newline ,stream-var)
       (json-encode-postfix ,postfix ,stream-var))))

(defun json-indent-implicit-encoding (string &key (start 0) (end (length string)) output)
  "Post-process STRING assuming it has been encoded in 'implicit mode'."
  (if (not output)
      (with-output-to-string (output)
	(json-indent-implicit-encoding string :start start :end end :output output))
      (loop with i = start
	    while (< i end)
	    for c = (char string i)
	    ;; do (warn "~D: ~S" i c)
	    do (incf i)
	       (cond
		 ((char= c %json-implicit-indent-marker%)
		  (multiple-value-bind (paren-index next-i)
		      (read-from-string string t nil :start i :end end)
		    (setf i next-i)
		    (let* ((close-marker (format nil "~C~C~D " #\tab %json-implicit-indent-marker% paren-index))
			   (close-position (or (search close-marker string :start2 i :end2 end)
					       (error "No matching close-paren ~S at position ~D." close-marker i)))
			   (*json-indent* (json-next-indent)))
		      (json-indent-implicit-encoding string :start i :end close-position :output output)
		      (setf i (+ close-position (length close-marker))))
		    (loop repeat (json-current-indent) do (write-char #\space output))))
		 ((char= c #\tab)
		  (loop repeat (json-current-indent) do (write-char #\space output)))
		 (t (write-char c output))))))

(defun json-object* (plist &rest properties-schema &key &allow-other-keys)
"Format PLIST as a JSON object with PROPERTIES-SCHEMA [<key> <sub-schema>]*,
where each key corresponds to a PLIST indicator and identifies the
sub-schema for that object property. Encoding follows the layout of
PLIST. Entries in PROPERTIES-SCHEMA but not in PLIST are
ignored. Entries in PLIST but not in PROPERTIES-SCHEMA are encoded
with the NIL schema."
  (if (not plist)
      "null"
      (with-json-encode-indentation (json plist "{" "}")
	(format json "~{~C~A~C:~A~A~^,~:/joysen:json-encode-newline/~:*~}"
		(loop for (k v) on plist by #'cddr
		      for property-schema = (getf properties-schema k)
		      nconc (list *json-quote*
				  (json-encode-keyword k)
				  *json-quote*
				  (json-encode-space)
				  (with-json-index-trace (k)
				    (encode v property-schema))))))))

(defun json-getter-object (value getter &rest properties-schema &key &allow-other-keys)
  "Format VALUE as a JSON object with PROPERTIES-SCHEMA [<property-name>
<property-schema>]*, where each PROPERTY-NAME is looked up in VALUE by
(GETTER <value> <property-name> <default>). The PROPERTY-SCHEMA
identifies the sub-schema for that property value. Encoding follows
the layout of PROPERTIES-SCHEMA. Entries in VALUE but not in
PROPERTIES-SCHEMA are ignored. Values missing from VALUE are
identified by GETTER returning the default value, and dealt with
according to the property's schema: JSON-OPTIONAL means a missing
property is not encoded at all, while JSON-REQUIRED means a missing
property is an error. A null VALUE is encoded as `\"null\"`."
  (cond
    ((not value)
     "null")
    ((json-encode-assert (not *json-implicit-mode*)))
    (t (with-json-encode-indentation (json value "{" "}")
	 (format json "~{~C~A~C:~A~A~^,~/joysen:json-encode-newline/~:*~}"
		 (loop with no-value = '#:missing-property
		       for (key property-schema) on properties-schema by #'cddr
		       for property-value = (funcall getter value key no-value)
		       nconc (flet ((property (value schema)
				      (list *json-quote*
					    (json-encode-keyword key)
					    *json-quote*
					    (json-encode-space)
					    (with-json-index-trace (key)
					      (encode value schema)))))
			       (cond
				 ((not (eq property-value no-value))
				  (property property-value property-schema))
				 ((typep property-schema '(or (eql json-optional) (cons (eql json-optional))))
				  nil)
				 ((typep property-schema '(or (eql json-required) (cons (eql json-required))))
				  (json-encode-error "Required JSON property ~S missing from ~S" key value))
				 (t (property nil property-schema))))))))))

(defun json-object (plist &rest properties-schema &key &allow-other-keys)
  "Format PLIST as a JSON object with PROPERTIES-SCHEMA [<key> <property-schema>]*,
where each key corresponds to a PLIST indicator and identifies the
sub-schema for that object property. Encoding follows
PROPERTIES-SCHEMA. Entries in PLIST but not in PROPERTIES-SCHEMA are
ignored. Entries in PROPERTIES-SCHEMA but not in PLIST are taken as
NIL, with two specially-interpreted exceptions for the propertie's
schema: JSON-OPTIONAL means a missing property is not encoded at all,
while JSON-REQUIRED means a missing property is an error. A null PLIST
is encoded as `\"null\"`."
  (if *json-implicit-mode*
      ;; JSON-OBJECT makes no sense in implicit mode, because there is
      ;; no schema layout to follow.
      (apply #'json-object* plist properties-schema)
      (apply #'json-getter-object plist #'getf properties-schema)))

(defun json-format (value format &rest format-args)
  "Explicitly FORMAT VALUE into a quoted string."
  (format nil "~C~?~C" *json-quote* format (list* value format-args) *json-quote*))

(defun json-format* (value format &rest format-args)
  "Explicitly FORMAT VALUE, but no quotes, e.g. for number formatting."
  (apply #'format nil format value format-args))

(defun json-choice (value first-choice &rest more-choices)
  "VALUE must be a string matching one of CHOICES."
  (let ((choices (list* first-choice more-choices)))
    (json-string (or (find value choices :test #'string-equal)
		     (json-encode-error "JSON bad choice ~S between ~{~S~^, ~}" value choices)))))

(defun json-map (value function schema)
  "Apply FUNCTION to VALUE, then format the result by SCHEMA."
  (etypecase function
    ((or function symbol)
     (encode (funcall function value) schema))
    ((cons symbol)
     (encode (apply (car function) value (cdr function))
	     schema))))

(defun json-yield (discard-value value)
  (declare (ignore discard-value))
  value)

(defun json-assert (value &optional schema)
  "Err if VALUE is NIL, otherwise proceed formatting VALUE by SCHEMA."
  (json-encode-assert (not *json-implicit-mode*))
  (json-encode-assert (not (null value)) (value))
  (encode value schema))

(defun json-optional (value schema)
  "An optional entry. This only makes sense in certain contexts,
e.g. JSON-OBJECT."
  ;; Actual checking is done within e.g. JSON-OBJECT.
  (encode value schema))

(defun json-required (value schema)
  "A required entry. This only makes sense in certain contexts,
e.g. JSON-OBJECT."
  ;; Actual checking is done within e.g. JSON-OBJECT.
  (encode value schema))

(defun format-json-trace (&optional (trace *json-encode-trace*))
  (when trace
    (let ((*print-length* 4)
	  (*print-level* 1)
	  (*print-escape* t))
      (format nil "~{~W[~S]~^ -> ~}" (reverse trace)))))

(defun json-dict (plist &optional element-schema)
  "This is essentially the same as an JSON-OBJECT, except the keys are
exact strings, the keys are arbitrary (not to any schema) and all the
values are formatted to the optional ELEMENT-SCHEMA."
  (if (null plist)
      "{}"
      (with-json-encode-indentation (json plist "{" "}")
	(format json "~{~C~A~C: ~A~^,~/joysen:json-encode-newline/~:*~}"
		(loop for (k v) on plist by #'cddr
		      collect *json-quote*
		      collect (string k)
		      collect *json-quote*
		      collect (with-json-index-trace (k)
				(encode v element-schema)))))))

(defun json-bool (value)
  "True if VALUE is non-NIL."
  (if value "true" "false"))

(defun json-string (value)
  "VALUE is printed into a string. The value NIL designates the empty
string."
  (format nil "~C~A~C" *json-quote* (or value "") *json-quote*))

(defun json-decimal (value &optional (precision 2))
  "Format VALUE as a decimal with PRECISION."
  (check-type value real)
  (if (zerop precision)
      (format nil "~D" (round value))
      (format nil "~,vF" precision value)))

(defun json-integer (value &optional min max)
  "VALUE is output as a basic integer."
  (json-encode-assert (typep value 'integer))
  (when min
    (json-encode-assert (<= min value)))
  (when max
    (json-encode-assert (<= value max)))
  (format nil "~D" value))

(defun json-array (sequence &optional element-schema)
  "A list or vector of elements with the same ELEMENT-SCHEMA."
  (json-encode-assert (typep sequence 'sequence) (sequence))
  (if (zerop (length sequence))
      "[]"
      (with-json-encode-indentation (json sequence "[" "]")
	(format json "~{~A~^,~/joysen:json-encode-newline/~:*~}"
		(etypecase sequence
		  (list
		   (loop for x in sequence for index upfrom 0
			 collect (with-json-index-trace (index)
				   (encode x element-schema))))
		  (vector
		   (loop for x across sequence for index upfrom 0
			 collect (with-json-index-trace (index)
				   (encode x element-schema)))))))))

(defun json-tuple (value &rest element-schemas)
  "An ordered list of elements with different ELEMENT-TYPEs."
  (check-type value list)
  (cond
    ((null value)
     "null")
    ((/= (length value) (length element-schemas))
     (json-encode-error "JSON tuple size schema ~S mismatch: ~S" element-schemas value))
    (t (with-json-encode-indentation (json value "[" "]")
	 (format json "~{~A~^,~/joysen:json-encode-newline/~:*~}"
		 (loop for x in value for element-schema in element-schemas for index upfrom 0
		       collect (with-json-index-trace (index)
				 (encode x element-schema))))))))

(defun encode (value schema &key ((:keyword *json-keyword-mode*) *json-keyword-mode*)
			      ((:indent *json-indent*) *json-indent*)
			      ((:null-schema *json-null-schema*) *json-null-schema*))
  "This is the main entry-point for encoding VALUE into a JSON string
according to SCHEMA."
  (etypecase schema
    (null
     (if (and *json-implicit-mode*
	      ;; Implicit mode means the value should already be
	      ;; encoded at this point.
	      (stringp value))
	 value
	 (loop for (match-type schema-by-type) in *json-null-schema*
	       when (typep value match-type)
		 return (encode value schema-by-type)
	       finally (error 'json-unknown-value :value value))))
    (symbol
     (funcall schema value))
    (list
     (apply (car schema) value (cdr schema)))))

