(asdf:defsystem #:cl-yatlp
  :description "Yet another tool for language processing"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "lazy-list")
               (:file "atn")
               (:file "common")

               (:module lexer
                :serial t
                :components ((:file "cond")
                             (:file "states")
                             (:file "creation")
                             (:file "transformation")
                             (:file "lexer")))

               (:module parser
                :serial t
                :components ((:file "states")
                             (:file "creation")
                             (:file "transformation")
                             (:file "parser")))))
