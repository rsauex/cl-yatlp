(uiop:define-package #:cl-yatlp/tests/lexer/transformation
  (:use #:cl #:cl-yatlp/src/atn #:cl-yatlp/src/lexer/creation
        #:cl-yatlp/src/lexer/cond
        #:cl-yatlp/tests/lexer/test-utils
        #:cl-yatlp/src/lexer/transformation #:lisp-unit2)
  (:import-from #:alexandria
                #:curry))

(in-package #:cl-yatlp/tests/lexer/transformation)

(defmacro deftest (name grammar main-test &rest rule-tests)
  `(define-test ,name
       (:tags '(:all :lexer-transformation))
     (with-lexer-test
       (with-atn (nfa->dfa (grammar->nfa ',grammar))
         ,@(mapcar (lambda (test)
                     `(funcall ,(second test) (@rule-state ',(first test)))) rule-tests)
         (funcall ,main-test (@extra :start))))
     t))

(deftest transformation.1
  ((rule frag)
   (frag #\a :fragment))
  (.simple-state
   -> #\a (.end-state rule)))

(deftest transformation.2
  ((rule1 "ab")
   (rule2 "ac"))
  (.simple-state
   -> #\a (.simple-state
           -> #\b (.end-state rule1)
           -> #\c (.end-state rule2))))

(deftest transformation.3
  ((rule1 (:+ #\a))
   (rule2 "ab"))
  (.simple-state
   -> #\a (.end-state rule1
           -> #\a (.end-state rule1
                   = loop
                   -> #\a loop)
           -> #\b (.end-state rule2))))


(deftest transformation.4
  ((rule1 (:+ #\a))
   (rule2 "aa"))
  (.simple-state
   -> #\a (.end-state rule1
           -> #\a (.end-state rule1
                   -> #\a (.end-state rule1
                           = loop
                           -> #\a loop)))))

(deftest transformation.5
  ((rule1 "aa")
   (rule2 (:+ #\a)))
  (.simple-state
   -> #\a (.end-state rule2
           -> #\a (.end-state rule1
                   -> #\a (.end-state rule2
                           = loop
                           -> #\a loop)))))

(deftest transformation.6
  ((rule ("/" (:+? (:or "//" (:~ #\/))) "/")))
  (.simple-state
   -> #\/ (.simple-state
           -> #\/ (.simple-state
                   -> #\/ (.simple-state
                           = loop1
                           -> #\/ (.end-state rule
                                   = end
                                   -> #\/ loop1)
                           -> (:~ #\/) (.simple-state
                                        = loop2
                                        -> #\/ end
                                        -> (:~ #\/) loop2)))
           -> (:~ #\/) loop2)))

(deftest transformation.7
  ((rule ("/" (:*? :any) "/")))
  (.simple-state
   -> #\/ (.simple-state
           -> (:~ #\/) (.simple-state
                        = loop
                        -> (:~ #\/) loop
                        -> #\/ (.end-state rule
                                = end))
           -> #\/ end)))
