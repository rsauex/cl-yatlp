(defpackage #:parser-states
  (:use #:cl #:atn)
  (:export #:simple-state
           #:rule-state
           #:lex-state
           #:str-state
           #:eps-state
           #:mimic-state
           
           #:simple-rule
           #:or-rule
           #:loop-rule
           #:s-loop-rule
           #:p-loop-rule))

(in-package #:parser-states)

(defstate simple-state (state) ())

(defstate rule-state (simple-state)
  ((rule :accessor state-rule
         :initarg :rule)))

(defstate lex-state (simple-state)
  ((lex :accessor state-lex
        :initarg :lex)))

(defstate str-state (simple-state)
  ((text :accessor state-str
         :initarg :str)))

(defstate eps-state (simple-state) ())

(defstate mimic-state (simple-state)
  ((rule :accessor state-mimic-rule
         :initarg :rule)))

(defrule simple-rule (rule) ())

(defrule or-rule (rule) ())

(defrule loop-rule (rule)
  ((delim :accessor loop-delim
          :initarg :delim)))

(defrule s-loop-rule (loop-rule) ())

(defrule p-loop-rule (loop-rule) ())
