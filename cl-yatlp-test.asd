(asdf:defsystem #:cl-yatlp-test
  :depends-on (#:cl-yatlp
               #:lisp-unit2)
  :components ((:module tests
                :serial t
                :components ((:file "atn")
                             (:file "lexer")))))
