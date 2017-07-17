(defpackage #:opt-rule-tests
  (:use #:cl
        #:lisp-unit2
        #:parser-tests
        #:cl-yatlp/transformations/opt-rule
        #:cl-yatlp/atn
        #:cl-yatlp/parser-states
        #:cl-yatlp/parser-creation))

(in-package #:opt-rule-tests)

(defun test-aux (grammar &rest test-cases)
  (with-atn (grammar->atn grammar)
    (add-opt-rules)
    (mapc #'funcall test-cases)))

(defmacro deftest (name grammar &rest test-cases)
  `(define-test ,name
       (:tags '(:all :opt-rule))
     (test-aux ',grammar ,@test-cases)))

(deftest opt-rule.1
    ((rule1 -> rule2?)
     (rule2 -> "abc"))
  (.or-rule 'rule1 '()
            (.rule-state 'rule2?
                         (.end-state 'rule1 nil)))
  (.or-rule 'rule2 '()
            (.str-state "abc"
                        (.end-state 'rule2 nil)))
  (.or-rule 'rule2? '()
            (.eps-state (.end-state 'rule2? nil))
            (.mimic-state 'rule2
                          (.end-state 'rule2? nil))))

(deftest opt-rule.2
    ((rule1 -> :^ rule2?)
     (rule2 -> "abc"))
  (.or-rule 'rule1 '()
            (.mimic-state 'rule2?
                          (.end-state 'rule1 nil)))
  (.or-rule 'rule2 '()
            (.str-state "abc"
                        (.end-state 'rule2 nil)))
  (.or-rule 'rule2? '()
            (.eps-state (.end-state 'rule2? nil))
            (.mimic-state 'rule2
                          (.end-state 'rule2? nil))))
