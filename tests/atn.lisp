(uiop:define-package #:cl-yatlp/tests/atn
  (:use #:cl #:cl-yatlp/src/atn #:lisp-unit2))

(in-package #:cl-yatlp/tests/atn)

(defmacro def-lexer-test (name grammar str &rest forms)
  `(define-test ,name
       (:tags '(:lexer))
     (let ((result (lexer (make-string-input-stream ,str) ,grammar)))
       (levery (lambda (get exp) (assert-equal get exp))
               result ',forms))))

(defun test-1-aux (test-fn tests)
  (let ((atn (funcall test-fn (make-atn))))
    (dolist (test tests)
      (destructuring-bind (id type &rest slots)
          test
        (let ((state (cl-yatlp/src/atn::get-state atn id)))
          (assert-typep type state)
          (loop for (slot value . rest) on slots by #'cddr
                do (let ((real-value (slot-value state slot)))
                     (assert-equal value real-value))))))))

(defmacro def-atn-test.1 (name form &rest tests)
  `(define-test ,name
       (:tags '(:all :atn))
     (test-1-aux (lambda (atn) ,form atn)
                 ',tests)))

(defun test-2-aux (test-fn tests)
  (let ((atn (funcall test-fn (make-atn))))
    (dolist (test tests)
      (destructuring-bind (id type &rest slots)
          test
        (let ((rule (cl-yatlp/src/atn::get-rule atn id)))
          (assert-typep type rule)
          (loop for (slot value . rest) on slots by #'cddr
                do (let ((real-value (slot-value rule slot)))
                     (assert-equal value real-value))))))))

(defmacro def-atn-test.2 (name form &rest tests)
  `(define-test ,name
       (:tags '(:all :atn))
     (test-2-aux (lambda (atn) ,form atn)
                 ',tests)))


(def-atn-test.1 creation.1
    (progn
      (cl-yatlp/src/atn::add-state atn 'a 'state :cond 'b :nexts '(c d))
      (cl-yatlp/src/atn::add-state atn 'b 'call-state :call-to 'a)
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'd)))
  (a state
     cl-yatlp/src/atn::cond b
     cl-yatlp/src/atn::nexts (c d))
  (b call-state
     cl-yatlp/src/atn::cond t
     cl-yatlp/src/atn::nexts nil
     cl-yatlp/src/atn::to a))

(def-atn-test.2 creation.2
    (progn
      (cl-yatlp/src/atn::add-rule atn 'a 'rule :state 'b :options '(c d))
      (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'b)))
  (a rule
     cl-yatlp/src/atn::state b
     cl-yatlp/src/atn::options (c d)))

(def-atn-test.1 with-atn.1
    (with-atn atn
      (@add-state 'a 'state :cond 'b :nexts '(c d))
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))
      (assert-typep 'state (@get-state 'a))
      (@add-state 'b 'call-state :call-to 'a)
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'b))
      (assert-typep 'call-state (@get-state 'b)))
  (a state
     cl-yatlp/src/atn::cond b
     cl-yatlp/src/atn::nexts (c d))
  (b call-state
     cl-yatlp/src/atn::cond t
     cl-yatlp/src/atn::nexts nil
     cl-yatlp/src/atn::to a))

(def-atn-test.2 with-atn.2
    (with-atn atn
      (@add-rule 'a 'rule :state 'b :options '(c d))
      (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))
      (assert-typep 'rule (@get-rule 'a)))
  (a rule
     cl-yatlp/src/atn::state b
     cl-yatlp/src/atn::options (c d)))

(define-test state-type.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-state 'a 'state :cond 'b :nexts '(c d))
    (assert-eq 'state (@state-type 'a))))

(define-test typep.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-state 'a 'state :cond 'b :nexts '(c d))
    (assert-true (@typep 'a 'state))))

(def-atn-test.1 remove.1
    (progn
      (cl-yatlp/src/atn::add-state atn 'a 'state :cond 'b :nexts '(c d))
      (assert-true (cl-yatlp/src/atn::get-state atn 'a))
      (cl-yatlp/src/atn::rem-state atn 'a)
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))))

(def-atn-test.2 remove.2
    (progn
      (cl-yatlp/src/atn::add-rule atn 'a 'rule :state 'b :options '(c d))
      (assert-true (cl-yatlp/src/atn::get-rule atn 'a))
      (cl-yatlp/src/atn::rem-rule atn 'a)
      (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))))

(def-atn-test.1 remove.3
    (progn
      (cl-yatlp/src/atn::add-state atn 'a 'state :cond 'b :nexts '(c d))
      (assert-true (cl-yatlp/src/atn::get-state atn 'a))
      (with-atn atn
        (@rem-state 'a)
        (assert-true (cl-yatlp/src/atn::get-state atn 'a))
        (assert-error 'error (@get-state 'a)))
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))))

(def-atn-test.2 remove.4
    (progn
      (cl-yatlp/src/atn::add-rule atn 'a 'rule :state 'b :options '(c d))
      (assert-true (cl-yatlp/src/atn::get-rule atn 'a))
      (with-atn atn
        (assert-true (@get-rule 'a))
        (@rem-rule 'a)
        (assert-true (cl-yatlp/src/atn::get-rule atn 'a))
        (assert-error 'error (@get-rule 'a)))
      (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))))

(def-atn-test.1 remove.5
    (progn
      (with-atn atn
        (@add-state 'a 'state :cond 'b :nexts '(c d))
        (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))
        (assert-true (@get-state 'a))
        (@rem-state 'a)
        (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))
        (assert-error 'error (@get-state 'a)))
      (assert-error 'error (cl-yatlp/src/atn::get-state atn 'a))))

(def-atn-test.2 remove.6
    (progn
      (with-atn atn
        (@add-rule 'a 'rule :state 'b :options '(c d))
        (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))
        (assert-true (@get-rule 'a))
        (@rem-rule 'a)
        (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))
        (assert-error 'error (@get-rule 'a)))
      (assert-error 'error (cl-yatlp/src/atn::get-rule atn 'a))))

(define-test states.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-state 'a 'state :cond 'b :nexts '(c d))
    (assert-equal '(a) (@states))))

(define-test states.2
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-state 'a 'state :cond 'b :nexts '(c d))
    (@add-state 'b 'state :cond 'b :nexts '(c d))
    (assert-equal '(a b) (@states))
    (@rem-state 'a)
    (assert-equal '(b) (@states))))

(define-test states.3
    (:tags '(:all :atn))
  (let ((atn (make-atn)))
    (cl-yatlp/src/atn::add-state atn 'a 'state :cond 'b :nexts '(c d))
    (with-atn atn
      (@add-state 'b 'state :cond 'b :nexts '(c d))
      (assert-equal '(a b) (@states))
      (@rem-state 'a)
      (assert-equal '(b) (@states)))))

(define-test rules.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-rule 'a 'rule :state 'b :options '(c d))
    (assert-equal '(a) (@rules))))

(define-test rules.2
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-rule 'a 'rule :state 'b :options '(c d))
    (@add-rule 'b 'rule :state 'b :options '(c d))
    (assert-equal '(a b) (@rules))
    (@rem-rule 'a)
    (assert-equal '(b) (@rules))))

(define-test rules.3
    (:tags '(:all :atn))
  (let ((atn (make-atn)))
    (cl-yatlp/src/atn::add-rule atn 'a 'rule :state 'b :options '(c d))
    (with-atn atn
      (@add-rule 'b 'rule :state 'b :options '(c d))
      (assert-equal '(a b) (@rules))
      (@rem-rule 'a)
      (assert-equal '(b) (@rules)))))

(define-test extra.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (setf (@extra 'a) 'b)
    (assert-eq 'b (@extra 'a))))

(define-test delayed-rule.1
    (:tags '(:all :atn))
  (with-atn (make-atn)
    (@add-state 'a 'call-state :call-to (delayed-rule 'rule1))
    (@add-rule 'rule1 'rule :state 'b :options nil)
    (@add-state 'b 'state)
    (assert-true (@typep (@state-call-to 'a) 'state))
    (assert-true (@typep (@state-call-to 'a) 'state))))

(define-test atn->dot.1
    (:tags '(:all :atn))
  (let ((atn (make-atn)))
    (with-atn atn
      (@add-state 'a 'state :nexts '(c))
      (@add-state 'c 'state :nexts '(a)))
    (assert-equal "digraph g {
A [label = \"A\\nSTATE [T]\"];
A -> {C};
C [label = \"C\\nSTATE [T]\"];
C -> {A};
}" (with-output-to-string (res)
     (atn->dot atn res)))))
