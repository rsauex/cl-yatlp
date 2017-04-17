(defpackage #:lexer-creation
  (:use #:cl #:alexandria #:cond #:atn #:lexer-states)
  (:export #:grammar->atn))

(in-package #:lexer-creation)

(defgeneric state-for (rule-type format state-name next-state))

(defmethod state-for ((rule-type (eql :char)) format state-name next-state)
  (@add-state state-name 'simple-state :cond format :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :any)) format state-name next-state)
  (@add-state state-name 'simple-state :cond t :nexts (list next-state)))

(defmethod state-for ((rule-type (eql ':r)) format state-name next-state)
  (@add-state state-name 'simple-state :cond format :nexts (list next-state)))

(defmethod state-for ((rule-type (eql ':or)) format state-name next-state)
  (@add-state state-name 'eps-state
              :nexts (mapcar (lambda (f)
                               (let ((choice (gensym)))
                                 (desr-rule->state f choice next-state)
                                 choice))
                             (rest format))))

(defmethod state-for ((rule-type (eql :seq)) format state-name next-state)
  (if (null (rest format))
      (desr-rule->state (first format) state-name next-state)
      (let ((seq-end (gensym)))
        (desr-rule->state (first format) state-name seq-end)
        (state-for :seq (rest format) seq-end next-state))))

(defmethod state-for ((rule-type (eql ':?)) format state-name next-state)
  (let ((present (gensym)))
    (@add-state state-name 'eps-state :nexts (list present next-state))
    (desr-rule->state (rest format) present next-state)))

(defmethod state-for ((rule-type (eql ':*)) format state-name next-state)
  (let ((body (gensym)))
    (@add-state state-name 'loop-state :nexts (list body next-state))
    (desr-rule->state (rest format) body state-name)))

(defmethod state-for ((rule-type (eql ':*?)) format state-name next-state)
  (let ((body (gensym)))
    (@add-state state-name 'ng-loop-state :nexts (list body next-state))
    (desr-rule->state (rest format) body state-name)))

(defmethod state-for ((rule-type (eql ':+)) format state-name next-state)
  (let ((body-end (gensym)))
    (@add-state body-end 'loop-state :nexts (list state-name next-state))
    (desr-rule->state (rest format) state-name body-end)))

(defmethod state-for ((rule-type (eql ':+?)) format state-name next-state)
  (let ((body-end (gensym)))
    (@add-state body-end 'ng-loop-state :nexts (list state-name next-state))
    (desr-rule->state (rest format) state-name body-end)))

(defmethod state-for ((rule-type (eql :call)) format state-name next-state)
  (@add-state state-name 'call-state :call-to (delayed-rule format) :nexts (list next-state)))

(defmethod state-for ((rule-type (eql ':~)) format state-name next-state)
  (@add-state state-name 'simple-state :cond format :nexts (list next-state)))

(defun desr-rule->state (format state-name next-state)
  (cond
    ((null format)
     (error "Wrong format"))
    ((characterp format)
     (state-for :char format state-name next-state))
    ((stringp format)
     (desr-rule->state (coerce format 'list) state-name next-state))
    ((eq :any format)
     (state-for format format state-name next-state))
    ((symbolp format)
     (state-for :call format state-name next-state))
    ((listp format)
     (if (member (first format)
                 '(:r :or :? :~ :* :*? :+ :+?))
       (state-for (first format) format state-name next-state)
       (state-for :seq format state-name next-state)))
    (t (error "Wrong rule format ~A" format))))

(defun rule->state (rule state-name)
  (let ((end-state (gensym))) 
    (destructuring-bind (name format &rest options)
        rule
      (@add-state end-state 'end-state :type name :options options)
      (desr-rule->state format state-name end-state)
      state-name)))

(defun grammar->atn (grammar)
  (let ((atn (make-atn))) 
    (with-atn atn 
      (let ((next-states (loop for rule in grammar
                               collect (let ((state (gensym)))
                                         (@add-rule (first rule) 'rule :state state :options (rest (rest rule)))
                                         (rule->state rule state)
                                         (unless (member :fragment (rest (rest rule)))
                                           state)))))
        (@add-state :start 'simple-state :nexts (remove-if #'null next-states))
        (setf (@extra :order)
              (loop for rule in grammar
                    collect (first rule)))
        atn))))
