(defsystem #:cl-yatlp
  :description "Yet another tool for language processing"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:cl-yatlp/src/all)
  :in-order-to ((test-op (load-op #:cl-yatlp/tests/all)))
  :perform (test-op (o c) (symbol-call :cl-yatlp/tests/all :test-suite)))

(defsystem #:cl-yatlp/tests
  :description "Tests for cl-yatlp"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:cl-yatlp/test/all))

(register-system-packages "cl-yatlp/src/all" '(:cl-yatlp))
