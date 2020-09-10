(uiop:define-package #:cl-yatlp/src/lexer/creation
  (:use #:cl #:alexandria #:cl-yatlp/src/lexer/cond #:cl-yatlp/src/atn #:cl-yatlp/src/lexer/states)
  (:export #:grammar->nfa))

(in-package #:cl-yatlp/src/lexer/creation)

;;; Creating NFA from lexer rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Method for each type of form in rule

(defgeneric state-for (rule-type form next-states &optional priority)
  (:documentation "Creates necessary states and returns a list with
entry state and an input cond."))

(defmacro def-state-for (type vars &rest body)
  "Macro for defining `state-for' methods. Each value in `vars' must be a symbol,
which will be \"let'ed\" to gensyms."
  `(defmethod state-for ((rule-type (eql ,type)) form next-states &optional (priority :normal))
     (let ,(mapcar (rcurry #'list '(gensym)) vars)
       ,@body)))

(def-state-for :char (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (make-next :state state-id :cond form :priority priority))

(def-state-for :any (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (make-next :state state-id :cond t :priority priority))

(def-state-for :r (state-id)
  (@add-state state-id 'simple-state :nexts next-states)
  (make-next :state state-id :cond form :priority priority))

(def-state-for :or (state-id)
  (@add-state state-id 'simple-state
              :nexts (mapcar (rcurry #'rule-form->state next-states priority)
                             (rest form)))
  (make-next :state state-id :cond :eps :priority priority))

(def-state-for :seq ()
  (if (null (rest form))
      (rule-form->state (first form) next-states priority)
      (rule-form->state (first form) (list (state-for :seq (rest form) next-states priority)) priority)))

(def-state-for :? (state-id)
  (let ((present (state-for :seq (rest form) next-states priority)))
    (@add-state state-id 'simple-state :nexts (cons present next-states))
    (make-next :state state-id :cond :eps :priority priority)))

(def-state-for :* (state-id)
  (let ((body (state-for :seq (rest form) (cons (make-next :state state-id :cond :eps :priority priority) next-states)  priority)))
    (@add-state state-id 'simple-state :nexts (cons body next-states))
    (make-next :state state-id :cond :eps :priority priority)))

(def-state-for :*? (state-id)
  (let ((body (state-for :seq (rest form) (list (make-next :state state-id :cond :eps :priority priority)) :low)))
    (@add-state state-id 'simple-state :nexts (append next-states (list body)))
    (make-next :state state-id :cond :eps :priority priority)))

(def-state-for :+ (body-end)
  (let ((body (state-for :seq (rest form) (list (make-next :state body-end :cond :eps :priority priority)) priority)))
    (@add-state body-end 'simple-state :nexts (cons body next-states))
    body))

(def-state-for :+? (start-id end-id)
  (let ((body (state-for :seq (rest form) (list (make-next :state end-id :cond :eps :priority priority)) :low)))
    (@add-state start-id 'simple-state :nexts (list body))
    (@add-state end-id 'simple-state :nexts (cons (make-next :state start-id :cond :eps :priority priority) next-states))
    (make-next :state start-id :cond :eps :priority priority)))

(def-state-for :call (state-id)
  (@add-state state-id 'call-state :call-to (delayed-rule form) :nexts next-states)
  (make-next :state state-id :cond :eps :priority priority))

(def-state-for :~ (state-id)
  (@add-state state-id 'simple-state :cond t :nexts next-states)
  (make-next :state state-id :cond form :priority priority))

(defun rule-form->state (form next-states priority)
  "Call the corresponding `state-for' method for the `form'."
  (cond
    ((null form)
     next-states)
    ((characterp form)
     (state-for :char form next-states priority))
    ((stringp form)
     (rule-form->state (coerce form 'list) next-states priority))
    ((eq :any form)
     (state-for :any form next-states priority))
    ((symbolp form)
     (state-for :call form next-states priority))
    ((listp form)
     (if (member (first form) '(:r :or :? :~ :* :*? :+ :+?))
         (state-for (first form) form next-states priority)
         (state-for :seq form next-states priority)))
    (t (error "Wrong rule form ~A" form))))

(defun rule->state (rule)
  "Transforms a given rule `rule' into a sequence of states in NFA.
Returns the first of them and the input cond."
  (let ((start-state (gensym))
        (end-state (gensym))) 
    (destructuring-bind (name form &rest options)
        rule
      (@add-state start-state 'simple-state :cond t :nexts (list (rule-form->state form `(,(make-next :state end-state :cond :eps)) :normal)))
      (@add-state end-state 'end-state :type name :options options)
      (make-next :state start-state :cond :eps))))

(defun grammar->start-states (grammar)
  "Creates and adds rule (in current atn) for each rule in `grammar'.
Tranfroms all the rules forms into corresponding states. Returns
a list of id of first states of each non-fragment rule."
  (loop for rule in grammar
        for (rule-name _ . rule-options) = rule
        for start-state = (rule->state rule)
        do (@add-rule rule-name 'rule :state (next-state start-state) :options rule-options)
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
