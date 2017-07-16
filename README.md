# cl-yatlp [![Build Status](https://travis-ci.org/rsauex/cl-yatlp.svg?branch=master)](https://travis-ci.org/rsauex/cl-yatlp) [![Coverage Status](https://coveralls.io/repos/github/rsauex/cl-yatlp/badge.svg?branch=master)](https://coveralls.io/github/rsauex/cl-yatlp?branch=master)
Yet another tool for language processing

Using this tool it is possible to build lexers and parsers (only LL(1) for now) in
an easy way.

Current features:
- basic lexer generator
- basic parser generator

Partly implemented:
- pretty printing AST into readable code

Future:
- tools for code matching and transformation (in AST form)
- extend parser to LL(*)
- normal error handling

## Lexer
The function for defining lexers is
```lisp
(deflexer <grammar-name>
  (<rule-name> <rule-form> <options>...)
  ...)
```

Rules are defined in a regex-like s-expressions:
- ```(<elements>...)``` - sequence, e.g. ```(#\a #\b #\c)```
- ```(:+ <elements>...)```
- ```(:+? <elements>...)```
- ```(:* <elements>...)```
- ```(:*? <elements>...)```
- ```(:or <elements>...)```
- ```(:? <elemtents>...)```
- ```(:r <form-char> <to-char>)``` - like ```[..-..]``` in regex.
- ```:any``` - like ```.``` in regex.

Options:
- ```:skip <bool>``` - if ```<bool>``` is ```t``` then resulting tokens will be omitted
- ```:fragment``` - the corresponding rule is not a independent rule, can be used in other rules by their names

Main entry point into lexer is
```lisp
(lexer <grammar-symbol> <stream>)
```

Result is a lazy-list where each element is token of the following form:
```lisp
(<type> <sym-representation> <line-number> <column-number>)
```
the last element is
```lisp
(:EOF NIL <line-number> <column-number>)
```

### Example
Lexer for identifiers:
```lisp
(deflexer identifiers
  (letter (:r #\a #\z) :fragment)
  (digit (:r #\0 #\9) :fragment)
  (identifier (letter (:* (:or digit letter))))
  (whitespace (:+ (:or #\Newline #\Space #\Return #\Vt #\Page #\Tab)) :skip t))
```

## Parser
The function for defining parsers is
```lisp
(defparser <grammar-name>
  (<rule-name> <rule-form> <options>...)
  ...)
```

Each rule-form can be one of the following:
- ```(<elements>...)``` - (sequence-form) each element can be either rule-name or string.
- ```(:or <elements>...)``` - (or-form) each element can be either rule-name, string, sequence-form, ```:eps``` (eps-form) or ```(:^ rule)``` (mimic-form).
- ```(:* <delimiter> <rule-name>)``` - (star-form) delimiter is either string, rule-name or ```:eps```.
- ```(:+ <delimiter> <rule-name>)``` - (plus-form) delimiter is either string, rule-name or ```:eps```.
- ```(:lex <lexer-rule>)``` - (lexer-rule-form).

Result of parsing is an AST, which has the following form:
```lisp
(<term-type> <children>...)
```

For all types of rule-forms except for or-form ```<term-type>``` will be ```<rule-name>``` of the corresponding rule. In or-form
each alternative has its own ```<term-type>``` if form of ```<rule-name>-<some-number>```.
Thanks to it all the literals (strings) are not saved in term (for memory saving reason), 
so the rule ```(rule1 "abc")``` will create terms like ```(rule1)``` but not ```(rule1 <grammar>::|123|)```.

When mimic-form is present in or-form then the result of the rule in mimic form is return as-is without wrapping.
The rule which is mentioned in mimic-form must have an option ```:mimic <rule-name>``` (TODO: get rid of it, because it can be found during code generation). 
e.g. for the following grammar
```lisp
(defparser some-grammar
  (rule1 -> "abc"
         -> :^ rule2))
  (rule2 -> "123"))
```
the result of parsing ```"123"``` will be ```(rule2)``` unlike ```(rule1 (rule2))``` which will be the result
of the similar grammar using ordinary rule instead of mimic one.
