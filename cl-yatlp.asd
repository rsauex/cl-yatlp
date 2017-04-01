(asdf:defsystem #:cl-yatlp
  :description "Yet another tool for language processing"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :depends-on (#:alexandria
               #:eager-future2)
  :serial t
  :components ((:file "test")))
