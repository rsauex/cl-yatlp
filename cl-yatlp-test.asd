(asdf:defsystem #:cl-yatlp-test
  :depends-on (:cl-yatlp
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "tests/lexer"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
