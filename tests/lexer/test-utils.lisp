(defpackage #:lexer/test-utils
  (:use #:cl #:cl-yatlp/atn #:cl-yatlp/lexer-states #:cl-yatlp/lexer-creation #:lisp-unit2)
  (:export #:with-lexer-test
           #:->
           #:=
           #:.simple-state
           #:.end-state
           #:.call-state
           #:.ng-loop-start
           #:.ng-loop-end))

(in-package #:lexer/test-utils)

(defvar *states*)

(defmacro with-lexer-test (&body body)
  `(let ((*states* (make-hash-table)))
     ,@body))

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
