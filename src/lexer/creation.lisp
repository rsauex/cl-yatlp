(defpackage #:cl-yatlp/lexer-creation
  (:use #:cl #:alexandria #:cl-yatlp/cond #:cl-yatlp/atn #:cl-yatlp/lexer-states)
  (:export #:grammar->nfa))

(in-package #:cl-yatlp/lexer-creation)

;;; Creating NFA from lexer rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Method for each type of form in rule

(defgeneric state-for (rule-type form next-states)
  (:documentation "Creates necessary states and returns a list with
entry state and an input cond."))

(defmacro def-state-for (type vars &rest body)
  "Macro for defining `state-for' methods. Each value in `vars' must be a symbol,
which will be \"let'ed\" to gensyms."
  `(defmethod state-for ((rule-type (eql ,type)) form next-states)
     (let ,(mapcar (rcurry #'list '(gensym)) vars)
       ,@body)))

(def-state-for :char (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (list state-id form))

(def-state-for :any (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (list state-id t))

(def-state-for :r (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (list state-id form))

(def-state-for :or (state-id)
  (@add-state state-id 'simple-state
              :nexts (mapcar (rcurry #'rule-form->state next-states)
                             (rest form)))
  (list state-id :eps))

(def-state-for :seq ()
  (if (null (rest form))
      (rule-form->state (first form) next-states)
      (rule-form->state (first form) (list (state-for :seq (rest form) next-states)))))

(def-state-for :? (state-id)
  (let ((present (state-for :seq (rest form) next-states)))
    (@add-state state-id 'simple-state :nexts (cons present next-states))
    (list state-id :eps)))

(def-state-for :* (state-id)
  (let ((body (state-for :seq (rest form) (cons (list state-id :eps) next-states))))
    (@add-state state-id 'simple-state :nexts (cons body next-states))
    (list state-id :eps)))

(def-state-for :*? (start-id end-id)
  (let ((body (state-for :seq (rest form) `((,end-id :eps)))))
    (@add-state start-id 'ng-loop-start :nexts (list body (list end-id :eps)))
    (@add-state end-id 'ng-loop-end :nexts (cons (list start-id :eps) next-states))
    (list start-id :eps)))

(def-state-for :+ (body-end)
  (let ((body (state-for :seq (rest form) `((,body-end :eps)))))
    (@add-state body-end 'simple-state :nexts (cons body next-states))
    body))

(def-state-for :+? (start-id end-id)
  (let ((body (state-for :seq (rest form) `((,end-id :eps)))))
    (@add-state start-id 'ng-loop-start :nexts (list body))
    (@add-state end-id 'ng-loop-end :nexts (cons (list start-id :eps) next-states))
    (list start-id :eps)))

(def-state-for :call (state-id)
  (@add-state state-id 'call-state :call-to (delayed-rule form) :nexts next-states)
  (list state-id :eps))

(def-state-for :~ (state-id)
  (@add-state state-id 'simple-state :cond t :nexts next-states)
  (list state-id form))

(defun rule-form->state (form next-states)
  "Call the corresponding `state-for' method for the `form'."
  (cond
    ((null form)
     next-states)
    ((characterp form)
     (state-for :char form next-states))
    ((stringp form)
     (rule-form->state (coerce form 'list) next-states))
    ((eq :any form)
     (state-for :any form next-states))
    ((symbolp form)
     (state-for :call form next-states))
    ((listp form)
     (if (member (first form) '(:r :or :? :~ :* :*? :+ :+?))
         (state-for (first form) form next-states)
         (state-for :seq form next-states)))
    (t (error "Wrong rule form ~A" form))))

(defun rule->state (rule)
  "Transforms a given rule `rule' into a sequence of states in NFA.
Returns the first of them and the input cond."
  (let ((start-state (gensym))
        (end-state (gensym))) 
    (destructuring-bind (name form &rest options)
        rule
      (@add-state start-state 'simple-state :cond t :nexts (list (rule-form->state form `((,end-state :eps)))))
      (@add-state end-state 'end-state :type name :options options)
      (list start-state :eps))))

(defun grammar->start-states (grammar)
  "Creates and adds rule (in current atn) for each rule in `grammar'.
Tranfroms all the rules forms into corresponding states. Returns
a list of id of first states of each non-fragment rule."
  (loop for rule in grammar
        for (rule-name _ . rule-options) = rule
        for start-state = (rule->state rule)
        do (@add-rule rule-name 'rule :state (first start-state) :options rule-options)
        unless (member :fragment rule-options)
          collect start-state))

(defun grammar->nfa (grammar)
  "For a given grammar creates an nondeterministic finite automaton (nfa)"
  (let ((nfa (make-atn))) 
    (with-atn nfa 
      (let ((start-id (gensym)))
        (@add-state start-id 'simple-state :nexts (grammar->start-states grammar))
        (setf (@extra :start) start-id)
        (setf (@extra :order) (mapcar #'first grammar))))
    nfa))
