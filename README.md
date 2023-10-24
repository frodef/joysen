# Joysen JSON encoding library for Common Lisp
### Frode Fjeld <frodevf@gmail.com>

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
  
### Example:

	(encode (list :foo-bar 42)
            '(json-object :foo-bar json-integer)
                          :keyword :camel)

## Implicit mode:

A lisp form is used to construct JSON output directly. There is no
explicit 'schema' object, rather the JSON mapping declarations are
intertwined with the lisp forms that generate the values. That is, the
JSON encoder functions are called as you normally would. The
with-implicit-json macro will then prettify the resulting output.

Implicit mode is useful for generating one-off JSON syntax that
doesn't correspond to any particular lisp objects/values.

### Usage:

	(JOYSEN:WITH-IMPLICIT-JSON (<options>) <form>)
	
### Example:

	(with-implicit-json (:keyword :camel)
      (json-object (list :foo-bar (json-integer 42))))

Each example encodes a JSON 'object' with one slot `'fooBar'` that is
to be encoded as an integer that happens to be 42.


## License

Unlicense

