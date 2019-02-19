(uiop:define-package #:cl-yatlp/tests/all
  (:use #:cl
        #:lisp-unit2
        ;; ATN tests
        #:cl-yatlp/tests/atn
        ;; Lexer tests
        #:cl-yatlp/tests/lexer/cond
        #:cl-yatlp/tests/lexer/creation
        #:cl-yatlp/tests/lexer/transformation
        ;; Parser tests
        #:cl-yatlp/tests/parser/transformations/mimic
        #:cl-yatlp/tests/parser/transformations/opt-rule))

(in-package #:cl-yatlp/tests/all)

(defun test-suite ()
  (with-summary ()
    (run-tests :tags :all)))

(defun run-test-suite ()
  (not (lisp-unit2:failed (test-suite))))
