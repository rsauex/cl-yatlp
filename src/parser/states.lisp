(defpackage #:cl-yatlp/parser-states
  (:use #:cl #:cl-yatlp/atn)
  (:export #:simple-state
           #:rule-state
           #:lex-state
           #:str-state
           #:eps-state
           #:mimic-state
           #:p-end-state
           
           #:simple-rule
           #:or-rule
           #:loop-rule
           #:s-loop-rule
           #:p-loop-rule))

(in-package #:cl-yatlp/parser-states)

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

(defstate p-end-state (end-state)
  ((format :accessor state-end-format
           :initarg :format
           :initform nil)))

(defrule simple-rule (rule) ())

(defrule or-rule (rule) ())

(defrule loop-rule (rule)
  ((delim :accessor loop-delim
          :initarg :delim)))

(defrule s-loop-rule (loop-rule) ())

(defrule p-loop-rule (loop-rule) ())
