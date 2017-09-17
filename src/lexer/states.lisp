(defpackage #:cl-yatlp/lexer-states
  (:use #:cl #:cl-yatlp/atn)
  (:export #:simple-state
           #:loop-state
           #:ng-loop-state
           #:eps-state
           #:ng-loop-start
           #:ng-loop-end))

(in-package #:cl-yatlp/lexer-states)

(defstate simple-state (state) ())

(defstate ng-loop-start (simple-state) ())
(defstate ng-loop-end (simple-state) ())

(defstate end-state (state)
  ((type :accessor state-end-type
         :initarg :type)
   (options :accessor state-end-options
            :initform nil
            :initarg :options)))
