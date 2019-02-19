(uiop:define-package #:cl-yatlp/tests/lexer/creation
  (:use #:cl #:cl-yatlp/tests/lexer/test-utils
        #:cl-yatlp/src/atn #:cl-yatlp/src/lexer/states
        #:cl-yatlp/src/lexer/creation #:lisp-unit2))

(in-package #:cl-yatlp/tests/lexer/creation)

(defmacro deftest (name grammar main-test &rest rule-tests)
  `(define-test ,name
       (:tags '(:all :lexer-creation))
     (with-lexer-test
       (with-atn (grammar->nfa ',grammar)
         ,@(mapcar (lambda (test)
                     `(funcall ,(second test) (@rule-state ',(first test)))) rule-tests)
         (funcall ,main-test (@extra :start))))
     t))

(deftest creation.1
    ((rule #\a))
  (.simple-state
   -> :eps (.simple-state
            -> #\a (.simple-state
                    -> :eps (.end-state rule)))))

(deftest creation.2
    ((rule "abc"))
  (.simple-state
   -> :eps (.simple-state
            -> #\a (.simple-state
                    -> #\b (.simple-state
                            -> #\c (.simple-state
                                    -> :eps (.end-state rule)))))))

(deftest creation.3
    ((rule :any))
  (.simple-state
   -> :eps (.simple-state
            -> t (.simple-state
                  -> :eps (.end-state rule)))))

(deftest creation.4
    ((rule rule1)
     (rule1 #\a :fragment))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.call-state a
                     -> :eps (.end-state rule))))
  (rule1
   (.simple-state
    = a
    -> #\a (.simple-state
            -> :eps (.end-state rule1)))))

(deftest creation.5
    ((rule (:r #\a #\c)))
  (.simple-state
   -> :eps (.simple-state
            -> (:r #\a #\c) (.simple-state
                             -> :eps (.end-state rule)))))

(deftest creation.6
    ((rule (:or #\a #\b #\c)))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.simple-state
                     -> #\a (.simple-state
                             -> :eps (.end-state rule))
                     -> #\b (.simple-state
                             -> :eps (.end-state rule))
                     -> #\c (.simple-state
                             -> :eps (.end-state rule))))))

(deftest creation.7
    ((rule (:? #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.simple-state
                     -> #\a (.simple-state
                             -> :eps (.end-state rule))
                     -> :eps (.end-state rule)))))

(deftest creation.8
    ((rule (:~ #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> (:~ #\a) (.simple-state
                         -> :eps (.end-state rule)))))

(deftest creation.9
    ((rule (:* #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.simple-state
                     = loop-start
                     -> #\a (.simple-state
                             -> :eps loop-start
                             -> :eps (.end-state rule
                                                 = end))
                     -> :eps end))))

(deftest creation.10
    ((rule (:*? #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.ng-loop-start
                     = loop-start
                     -> #\a (.simple-state
                             -> :eps (.ng-loop-end
                                      = loop-end
                                      -> :eps loop-start
                                      -> :eps (.end-state rule)))
                     -> :eps loop-end))))

(deftest creation.11
    ((rule (:+ #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> #\a (.simple-state
                    = body
                    -> :eps (.simple-state
                             -> #\a body
                             -> :eps (.end-state rule))))))

(deftest creation.12
    ((rule (:+? #\a)))
  (.simple-state
   -> :eps (.simple-state
            -> :eps (.ng-loop-start
                     = loop-start
                     -> #\a (.simple-state
                             -> :eps (.ng-loop-end
                                      -> :eps loop-start
                                      -> :eps (.end-state rule)))))))
