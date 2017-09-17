(defpackage #:lexer/creation-tests
  (:use #:cl #:cl-yatlp/atn #:cl-yatlp/lexer-states #:cl-yatlp/lexer-creation #:lisp-unit2))

(in-package #:lexer/creation-tests)

(defvar *states*)

(defmacro deftest (name grammar main-test &rest rule-tests)
  `(define-test ,name
       (:tags '(:all :lexer-creation))
     (let ((*states* (make-hash-table)))
       (with-atn (grammar->nfa ',grammar)
         ,@(mapcar (lambda (test)
                     `(funcall ,(second test) (@rule-state ',(first test)))) rule-tests)
         (funcall ,main-test (@extra :start))))
     t))

(defun .%check-nexts (&rest nexts)
  (lambda (state)
    (mapc
     (lambda (next1 next2)
       (assert-equalp (second next1) (second next2))
       (funcall (first next1) (first next2)))
     nexts (@state-nexts state))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun forms->nexts (forms &optional acc)
    (if forms 
        (progn
          (assert-eq '-> (first forms))
          (forms->nexts (nthcdr 3 forms)
                        (cons `(list ,(if (symbolp (third forms))
                                          `(lambda (state)
                                             (assert-true (@same-ids? (gethash ',(third forms) *states*) state)))
                                          (third forms))
                                     ',(second forms)) acc)))
        (reverse acc)))

  (defun to-test-fn (name forms)
    `(,name
      ,@(forms->nexts forms))))

(defmacro def-check (name params &body body)
  `(defmacro ,name (,@params &rest nexts)
     (if (eq '= (first nexts))
         (progn
           (assert-typep 'symbol (second nexts))
           `(lambda (state)
              (setf (gethash ',(second nexts) *states*) state)
              ,@,(cons 'list body)
              (funcall
               ,(to-test-fn '.%check-nexts (rest (rest nexts)))
               state)))
         `(lambda (state)
            ,@,(cons 'list body)
            (funcall
             ,(to-test-fn '.%check-nexts nexts)
             state)))))

(def-check .simple-state ()
  `(assert-true (@typep state 'simple-state)))

(def-check .end-state (type)
  `(progn
     (assert-true (@typep state 'end-state))
     (assert-eq ',type (@state-end-type state))))

(def-check .call-state (state)
  `(progn
     (assert-true (@typep state 'call-state))
     (assert-true (@same-ids? (gethash ',state *states*) (@state-call-to state)))))

(def-check .ng-loop-start ()
  `(assert-true (@typep state 'ng-loop-start)))

(def-check .ng-loop-end ()
  `(assert-true (@typep state 'ng-loop-end)))

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
