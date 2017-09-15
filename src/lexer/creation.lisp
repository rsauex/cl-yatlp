(defpackage #:cl-yatlp/lexer-creation
  (:use #:cl #:alexandria #:cl-yatlp/cond #:cl-yatlp/atn #:cl-yatlp/lexer-states)
  (:export #:grammar->nfa))

(in-package #:cl-yatlp/lexer-creation)

(defgeneric state-for (rule-type format next-states ))

(defmacro def-state-for (type &rest body)
  `(defmethod state-for ((rule-type (eql ,type)) form next-states)
     ,@body))

(def-state-for :char
  (let ((state-id (gensym)))
    (@add-state state-id 'simple-state :cond t :nexts next-states)
    (list state-id form)))

(def-state-for :any
  (let ((state-id (gensym)))
    (@add-state state-id 'simple-state :cond t :nexts next-states)
    (list state-id t)))

(def-state-for :r
  (let ((state-id (gensym)))
    (@add-state state-id 'simple-state :cond t :nexts next-states)
    (list state-id form)))

(def-state-for :or
  (let ((state-id (gensym)))
    (@add-state state-id 'simple-state
                :cond t
                :nexts (mapcar (lambda (f)
                                 (rule-form->state f next-states))
                               (rest form)))
    (list state-id :eps)))

(def-state-for :seq
  (if (null (rest form))
      (rule-form->state (first form) next-states)
      (rule-form->state (first form) (list (state-for :seq (rest form) next-states)))))

(def-state-for :?
  (let ((state-id (gensym))
        (present (state-for :seq (rest form) next-states)))
    (@add-state state-id 'simple-state :cond t :nexts (cons present next-states))
    (list state-id :eps)))

(def-state-for :*
  (let* ((state-id (gensym))
         (body (state-for :seq (rest form) (cons (list state-id :eps) next-states))))
    (@add-state state-id 'simple-state :cond t :nexts (cons body next-states))
    (list state-id :eps)))

(def-state-for :*?
  (let* ((state-id (gensym))
         (body (state-for :seq (rest form) (cons (list state-id :eps) next-states))))
    (@add-state state-id 'simple-state :cond t :nexts (cons body next-states))
    (list state-id :eps)))

(def-state-for :+
  (let* ((body-end (gensym))
         (body (state-for :seq (rest form) `((,body-end :eps)))))
    (@add-state body-end 'simple-state :cond t :nexts (cons body next-states))
    body))

(def-state-for :+?
  (let* ((body-end (gensym))
         (body (state-for :seq (rest form) `((,body-end :eps)))))
    (@add-state body-end 'simple-state :cond t :nexts (cons body next-states))
    body))

(def-state-for :call
  (let ((state-id (gensym)))
    (@add-state state-id 'call-state :call-to (delayed-rule form) :nexts next-states)
    (list state-id :eps)))

(def-state-for :~
  (let ((state-id (gensym)))
    (@add-state state-id 'simple-state :cond t :nexts next-states)
    (list state-id form)))

(defun rule-form->state (form next-states)
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
  "Transforms a given rule `rule' into a sequence of states in atn.
Returns the first of them."
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


(def-state-generic state->dot (state stream)
  (:documentation
   "Output state's representation in dot format into stream"))

(def-state-method state->dot ((state state) stream)
  (format stream "  ~A [label = \"~A\\n~A [~A]\"];~%"
          state state (type-of (@get-state state)) (@state-cond state))
  (dolist (x (@state-nexts state))
    (format stream "  ~A -> ~A [label=\"~A\"]~%"
            state (first x) (second x))))

(def-state-method state->dot ((state end-state) stream)
  (format stream "  ~A [peripheries=2 label=\"~A\\n~A [~A]\\ntype = ~A\"];~%"
          state state (@state-type state) (@state-cond state) (@state-end-type state)))

(defun @latn->dot (&optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (format stream "digraph g {~%")
  (@traverse-atn (rcurry #'state->dot stream))
  (format stream "}"))

(defun latn->dot (atn &optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (with-atn atn
    (@latn->dot stream)))
