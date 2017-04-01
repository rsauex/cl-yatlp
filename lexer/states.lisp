(defpackage #:lexer-states
  (:use #:cl #:atn)
  (:export #:simple-state
           #:loop-state
           #:ng-loop-state
           #:eps-state))

(in-package #:lexer-states)

(defclass simple-state (state) ())

(defclass eps-state (state) ())

(defclass loop-state (state) ())

(defclass ng-loop-state (loop-state) ())
