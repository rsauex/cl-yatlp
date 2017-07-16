(defpackage #:cl-yatlp/lexer-states
  (:use #:cl #:cl-yatlp/atn)
  (:export #:simple-state
           #:loop-state
           #:ng-loop-state
           #:eps-state))

(in-package #:cl-yatlp/lexer-states)

(defstate simple-state (state) ())

(defstate eps-state (state) ())

(defstate loop-state (state) ())

(defstate ng-loop-state (loop-state) ())
