# Joysen JSON encoding library for Common Lisp
### Frode Fjeld `<frodevf@gmail.com>`

A mechanism for convenient encoding (nested) lisp objects into strings
in JSON syntax.

This library operates in two modes: *Normal* or *Implicit* mode.

## Normal (explicit) mode:

Encoding happens by the (lisp) value and a schema as two separate
entities, as provided to `JOYSEN:ENCODE`. The value is an arbitrary
lisp object, and the schema (a list tree structure) describes how that
value is mapped to JSON syntax. The lisp value and the schema are
traversed in parallell while generating JSON output.

The schema object is a (lisp list) tree structure where the nodes name
JSON encoder functions that map a lisp value to a (JSON) string. Each
such function takes (implicitly) the lisp value as its first argument,
and optionally more parameters. In the schema, an encoder function is
named either by its symbol (which is applied to a single argument: the
lisp value) or it is named by a list `(<symbol> <args*>)`, in which
case the symbol is applied to the lisp value and the provided
ARGS. The ARGS can hold further sub-schemas, thus creating the tree
structure.

The Joysen library provides encoder functions for typical mappings,
e.g. integers `(joysen:json-integer)`, plists to 'objects'
`(joysen:json-object)`, lists or vectors to arrays
`(joysen:json-array)`, etc. However you can easily provide and use
your own encoder functions that map your objects either to calls to
Joysen encoder functions or directly to (JSON) strings.

Normal mode is useful for encoding values used otherwise in lisp code,
and to give those a JSON representation.

### Usage:

	(JOYSEN:ENCODE <value> <schema>)
  
### Basic examples:

    > (joysen:encode 42 'json-integer) ; basic schema
    "42"
    > (joysen:encode 42 'json-decimal) ; same value, different schema
    "42.00"
    > (joysen:encode 42 '(json-decimal 5)) ; optional precision parameter
    "42.00000"

### Composite schema example:

	> (princ
        (encode (list :foo-bar 42)
               '(json-object :foo-bar json-integer)
               :keyword :camel))

Output:

    {
       "fooBar": 42
    }

In this example, the plist `(:foo-bar 42)` is the lisp value that is
encoded according to the schema `(json-object :foo-bar
json-integer)`. There can of course be more properties, also with
composite schemas like more objects, arrays, etc.

## Implicit mode:

A lisp form is used to construct JSON output directly. There is no
explicit 'schema' object, rather the JSON mapping declarations
(i.e. function calls) are intertwined with the lisp code that generate
the content. That is, the JSON encoder functions are called as you
normally would, with no schema traversal. The `WITH-IMPLICIT-JSON`
macro will then prettify the resulting output.

Implicit mode is useful for generating one-off JSON syntax that
doesn't correspond to any particular lisp objects/values.

### Usage:

	(JOYSEN:WITH-IMPLICIT-JSON ([keyword options]*) <forms>)
	
### Example:

	> (princ
        (with-implicit-json (:keyword :camel)
          (json-object (list :foo-bar (json-integer 42)))))
	  
Output:

    {
       "fooBar": 42
    }

In this example, the `JSON-OBJECT` form will output the JSON object
with the `fooBar` property. There are only ephemeral lisp values, and
never any object with a `foo-bar' value of value the integer 42.

## Encoder functions

### `defun json-object (plist &rest properties-schema &key &allow-other-keys)`

Format `plist` as a JSON object with `properties-schema` `[<key>
<sub-schema>]*`, where each `key` corresponds to a `plist` indicator
and identifies the sub-schema for that object property. Encoding
follows `properties-schema`. Entries in `plist` but not in
`properties-schema` are ignored. Entries in `properties-schema` but
not in `plist` are taken as NIL, with two specially-interpreted
exceptions for the propertie's schema: `json-optional` means a missing
property is not encoded at all, while `json-required` means a missing
property is an error. A null `plist` is encoded as `"null"`.

### `defun json-array (sequence &optional element-schema)`

A list or vector of elements with the same `element-schema` encoded as
a JSON array.

### `defun json-decimal (value &optional (precision 2))`

Format `value` as a decimal with `precision`.

### `defun json-integer (value &optional min max)`
`value` is output as a basic integer (in decimal), optinally `min` and
`max` are checked.."

### `defun json-string (value)`

`value` is printed into a string. The value `NIL` designates the empty
string.

### `defun json-dict (plist &optional element-schema)`

This is essentially the same as an JSON-OBJECT, except the keys are
exact strings, the keys are arbitrary (not to any schema) and all the
values are formatted to the optional ELEMENT-SCHEMA.

### `defun json-bool (value)`
True if VALUE is non-NIL, otherwise false.
  
## License

Unlicense

