(defpackage #:lexer-states
  (:use #:cl #:atn)
  (:export #:simple-state
           #:loop-state
           #:ng-loop-state
           #:eps-state))

(in-package #:lexer-states)

(defstate simple-state (state) ())

(defstate eps-state (state) ())

(defstate loop-state (state) ())

(defstate ng-loop-state (loop-state) ())
