(uiop:define-package #:cl-yatlp/src/common
  (:use #:cl)
  (:export #:*lexer-grammars*
           #:*parser-grammars*))

(in-package #:cl-yatlp/src/common)

(defparameter *lexer-grammars* (make-hash-table))
(defparameter *parser-grammars* (make-hash-table))
