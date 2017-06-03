(defpackage #:common
  (:use #:cl)
  (:export #:*lexer-grammars*
           #:*parser-grammars*))

(in-package #:common)

(defparameter *lexer-grammars* (make-hash-table))
(defparameter *parser-grammars* (make-hash-table))
