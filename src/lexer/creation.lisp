(defpackage #:cl-yatlp/lexer-creation
  (:use #:cl #:alexandria #:cl-yatlp/cond #:cl-yatlp/atn #:cl-yatlp/lexer-states)
  (:export #:grammar->nfa))

(in-package #:cl-yatlp/lexer-creation)

(defgeneric state-for (rule-type format state-name next-state))

(defmacro def-state-for (type &rest body)
  `(defmethod state-for ((rule-type (eql ,type)) form state-id next-state)
     ,@body))

(def-state-for :char
  (@add-state state-id 'simple-state :cond form :nexts (list next-state)))

(def-state-for :any
  (@add-state state-id 'simple-state :cond t :nexts (list next-state)))

(def-state-for :r
  (@add-state state-id 'simple-state :cond form :nexts (list next-state)))

(def-state-for :or
  (@add-state state-id 'eps-state
              :nexts (mapcar (lambda (f)
                               (let ((alter (gensym)))
                                 (rule-form->state f alter next-state)
                                 alter))
                             (rest form))))

(def-state-for :seq
  (if (null (rest form))
      (rule-form->state (first form) state-id next-state)
      (let ((next-elem (gensym)))
        (rule-form->state (first form) state-id next-elem)
        (state-for :seq (rest form) next-elem next-state))))

(def-state-for :?
  (let ((present (gensym)))
    (@add-state state-id 'eps-state :nexts (list present next-state))
    (state-for :seq (rest form) present next-state)))

(def-state-for :*
  (let ((body (gensym)))
    (@add-state state-id 'loop-state :nexts (list body next-state))
    (state-for :seq (rest form) body state-id)))

(def-state-for :*?
  (let ((body (gensym)))
    (@add-state state-id 'ng-loop-state :nexts (list body next-state))
    (state-for :seq (rest form) body state-id)))

(def-state-for :+
  (let ((body-end (gensym)))
    (@add-state body-end 'loop-state :nexts (list state-id next-state))
    (state-for :seq (rest form) state-id body-end)))

(def-state-for :+?
  (let ((body-end (gensym)))
    (@add-state body-end 'ng-loop-state :nexts (list state-id next-state))
    (state-for :seq (rest form) state-id body-end)))

(def-state-for :call
  (@add-state state-id 'call-state :call-to (delayed-rule form) :nexts (list next-state)))

(def-state-for :~
  (@add-state state-id 'simple-state :cond form :nexts (list next-state)))

(defun rule-form->state (form state-id next-state)
  (cond
    ((null form)
     (error "Rule form must not be NIL"))
    ((characterp form)
     (state-for :char form state-id next-state))
    ((stringp form)
     (rule-form->state (coerce form 'list) state-id next-state))
    ((eq :any form)
     (state-for :any form state-id next-state))
    ((symbolp form)
     (state-for :call form state-id next-state))
    ((listp form)
     (if (member (first form) '(:r :or :? :~ :* :*? :+ :+?))
         (state-for (first form) form state-id next-state)
         (state-for :seq form state-id next-state)))
    (t (error "Wrong rule form ~A" form))))

(defun rule->state (rule)
  "Transforms a given rule `rule' into a sequence of states in atn.
Returns the first of them."
  (let ((start-state (gensym))
        (end-state (gensym))) 
    (destructuring-bind (name form &rest options)
        rule
      (@add-state end-state 'end-state :type name :options options)
      (rule-form->state form start-state end-state)
      start-state)))

(defun grammar->start-states (grammar)
  "Creates and adds rule (in current atn) for each rule in `grammar'.
Tranfroms all the rules forms into corresponding states. Returns
a list of id of first states of each non-fragment rule."
  (loop for rule in grammar
        for (rule-name _ . rule-options) = rule
        for start-state = (rule->state rule)
        do (@add-rule rule-name 'rule :state start-state :options rule-options)
        unless (member :fragment rule-options)
          collect start-state))

(defun grammar->nfa (grammar)
  "For a given grammar creates an nondeterministic finite automaton (nfa)"
  (let ((nfa (make-atn))) 
    (with-atn nfa 
      (@add-state :start 'simple-state :nexts (grammar->start-states grammar))
      (setf (@extra :order) (mapcar #'first grammar)))
    nfa))
