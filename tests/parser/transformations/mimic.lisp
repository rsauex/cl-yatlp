(defpackage #:mimic-tests
  (:use #:cl
        #:lisp-unit2
        #:cl-yatlp/transformations/mimic
        #:cl-yatlp/atn
        #:cl-yatlp/parser-states
        #:cl-yatlp/parser-creation))

(in-package #:mimic-tests)

(defmacro deftest (name grammar &rest body)
  `(define-test ,name
       (:tags '(:all :mimic))
     (with-atn (grammar->atn ',grammar)
       (add-mimics)
       ,@body)))

(deftest mimic.1
  ((rule1 -> :^ rule2)
   (rule2 -> "abc"))
  (assert-equal '(rule1) (getf (@rule-options 'rule2) :mimic)))

(deftest mimic.2
  ((rule1 -> "abc"
          -> :^ rule2)
   (rule2 -> "def"))
  (assert-equal '(rule1) (getf (@rule-options 'rule2) :mimic)))

(deftest mimic.3
  ((rule1 -> "abc"
          -> :^ rule3)
   (rule2 -> :^ rule3)
   (rule3 -> "def"))
  (assert-equal '(rule1 rule2) (getf (@rule-options 'rule3) :mimic)))
