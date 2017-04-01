(defpackage #:parser-states
  (:use #:cl #:atn)
  (:export #:simple-state
           #:eps-state
           #:loop-state
           #:s-loop-state
           #:p-loop-state))

(in-package #:parser-states)

(defclass simple-state (state) ())

(defclass eps-state (state) ())

(defclass loop-state (state) ())

(defclass s-loop-state (loop-state) ())

(defclass p-loop-state (loop-state) ())
