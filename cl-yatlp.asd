(asdf:defsystem #:cl-yatlp
  :description "Yet another tool for language processing"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :depends-on (#:alexandria
               #:eager-future2)
  :serial t
  :components ((:file "lazy-list")
               (:file "atn")

               (:file "lexer/cond")
               (:file "lexer/states")
               (:file "lexer/creation")
               (:file "lexer/transformation")
               (:file "lexer/lexer")

               (:file "parser/states")
               (:file "parser/creation")
               (:file "parser/transformation")
               (:file "parser/parser")))
