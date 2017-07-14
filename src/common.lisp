(defpackage #:cl-yatlp/common
  (:use #:cl)
  (:export #:*lexer-grammars*
           #:*parser-grammars*))

(in-package #:cl-yatlp/common)

(defparameter *lexer-grammars* (make-hash-table))
(defparameter *parser-grammars* (make-hash-table))
