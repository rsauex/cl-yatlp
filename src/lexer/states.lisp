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

(defstate eps-state (state) ())

(defstate loop-state (state) ())

(defstate ng-loop-state (loop-state) ())


(defstate ng-loop-start (simple-state) ())
(defstate ng-loop-end (simple-state) ())
