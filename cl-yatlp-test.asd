(asdf:defsystem #:cl-yatlp-test
  :depends-on (#:cl-yatlp
               #:small-tests)
  :components ((:file "tests/lexer")))
