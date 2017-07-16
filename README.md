# cl-yatlp [![Build Status](https://travis-ci.org/rsauex/cl-yatlp.svg?branch=master)](https://travis-ci.org/rsauex/cl-yatlp) [![Coverage Status](https://coveralls.io/repos/github/rsauex/cl-yatlp/badge.svg?branch=master)](https://coveralls.io/github/rsauex/cl-yatlp?branch=master)
Yet another tool for language processing

This tool is intended for easy lexer and parser building, code transformations and anlysis.

Current features:
- lexer generator
- LL(1) parser generator

Future:
- tools for code matching and transformation (in AST form)
- extend parser to LL(*)

## Lexer
The function for defining lexers is
```lisp
(deflexer <grammar-name>
  (<rule-name> <rule-form> <options>...)
  ...)
```

Rules are defined using regex-like s-expressions based syntax:
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
- ```:skip <bool>``` - if ```<bool>``` is ```t``` the result of the rule will be omitted
- ```:fragment``` - the corresponding rule is not an independent rule. Such rules are intended
to be referenced by other rules.

Main entry point into lexer is
```lisp
(lexer <stream> <grammar-keyword>)
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
  (<rule-name> <alternative>... [:options <options>...])
  ...)
```

Each alternative can be one of the following:
- ```-> <element>...``` - (simple-form) each element can be either rule-name or string.
- ```-> :^ <rule-name>``` - (mimic-form).
- ```-> :eps``` - (eps-form).
- ```-> * <delimiter> <rule-name>``` - (star-form) delimiter is either string, rule-name or ```:eps```.
- ```-> + <delimiter> <rule-name>``` - (plus-form) delimiter is either string, rule-name or ```:eps```.
- ```-> :lex <lexer-rule>``` - (lexer-rule-form).

When star-form, plus-form or lexer-rule-form is specified it must be the only alternative in the rule.

Result of parsing is an AST, which has the following form:
```lisp
(<term-type> <children>...)
```

Each alternative will have its own ```<term-type>``` in the following form: ```<rule-name>-<some-number>```.
Child is always either a term (for a rule-name in grammar) or a symbol (for lexer-rule-form). None of the
literals mentioned in rules are preserved in the resulting terms.

When mimic-form is present as one of alternative then the result of the rule in mimic form is return as-is without wrapping.
For example using the following grammar
```lisp
(defparser some-grammar
  (rule1 (:or "abc"
              (:^ rule2)))
  (rule2 ("123")
         :mimic rule1))
```
the result of parsing ```"123"``` will be ```(rule2)``` not ```(rule1 (rule2))``` which would be the result
of the similar grammar without mimic rule.
