(uiop:define-package #:cl-yatlp/tests/parser/test-utils
  (:use #:cl
        #:lisp-unit2
        #:cl-yatlp/src/atn
        #:cl-yatlp/src/parser/states
        #:cl-yatlp/src/parser/creation)
  (:export #:.or-rule
           #:.rule-state
           #:.str-state
           #:.mimic-state
           #:.eps-state
           #:.end-state))

(in-package #:cl-yatlp/tests/parser/test-utils)

(defun .or-rule (rule options &rest states)
  (lambda ()
    (assert-typep 'or-rule (@get-rule rule))
    (assert-equal options (@rule-options rule))
    (assert-equal (length states) (length (@rule-state rule)))
    (mapc (lambda (s-test s)
            (funcall s-test s))
          states
          (@rule-state rule))))

(defun .rule-state (rule &rest nexts)
  (lambda (state)
    (assert-typep 'rule-state (@get-state state))
    (assert-eq rule (@state-rule state))
    (assert-equal (length nexts) (length (@state-nexts state)))
    (mapc (lambda (s-test s)
            (funcall s-test s))
          nexts
          (@state-nexts state))))

(defun .str-state (str &rest nexts)
  (lambda (state)
    (assert-typep 'str-state (@get-state state))
    (assert-equal str (@state-str state))
    (assert-equal (length nexts) (length (@state-nexts state)))
    (mapc (lambda (s-test s)
            (funcall s-test s))
          nexts
          (@state-nexts state))))

(defun .mimic-state (rule &rest nexts)
  (lambda (state)
    (assert-typep 'mimic-state (@get-state state))
    (assert-equal rule (@state-mimic-rule state))
    (assert-equal (length nexts) (length (@state-nexts state)))
    (mapc (lambda (s-test s)
            (funcall s-test s))
          nexts
          (@state-nexts state))))

(defun .eps-state (&rest nexts)
  (lambda (state)
    (assert-typep 'eps-state (@get-state state))
    (assert-equal (length nexts) (length (@state-nexts state)))
    (mapc (lambda (s-test s)
            (funcall s-test s))
          nexts
          (@state-nexts state))))

(defun .end-state (type options)
  (lambda (state)
    (assert-typep 'end-state (@get-state state))
    (assert-equal type (@state-end-type state))
    (assert-equal options (@state-end-options state))))
