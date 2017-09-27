(asdf:defsystem #:cl-yatlp-test
  :depends-on (#:cl-yatlp
               #:lisp-unit2)
  :components ((:module tests
                :serial t
                :components ((:file "atn")
                             (:file "parser/parser-tests")
                             (:file "parser/transformations/mimic")
                             (:file "parser/transformations/opt-rule")
                             (:file "lexer/test-utils")
                             (:file "lexer/creation")
                             (:file "lexer/cond")
                             (:file "lexer/transformation")))))
