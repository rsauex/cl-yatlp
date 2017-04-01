(defpackage #:parser
  (:use #:cl #:alexandria #:cond #:atn #:parser-states #:lazy-list)
  (:export ;;#:defparser
           ;;#:parser
   ))

(in-package #:parser)

(defgeneric state-for (rule-type format state-name next-state))

(defmethod state-for ((rule-type (eql :str)) format state-name next-state)
  (@add-state state-name 'simple-state :cond format :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :eps)) format state-name next-state)
  (@add-state state-name 'simple-state :cond format :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :lex)) format state-name next-state)
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

(defmethod state-for ((rule-type (eql ':*)) format state-name next-state)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-end (gensym)))
      (@add-state body-end 's-loop-state :nexts (list state-name next-state) 
                                         :delim delim)
      (desr-rule->state body state-name body-end))))

(defmethod state-for ((rule-type (eql ':+)) format state-name next-state)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-end (gensym)))
      (@add-state body-end 'p-loop-state :nexts (list state-name next-state) 
                                         :delim delim)
      (desr-rule->state body state-name body-end))))

(defmethod state-for ((rule-type (eql :call)) format state-name next-state)
  (@add-state state-name 'call-state :call-to (delayed-rule format) :nexts (list next-state)))

(defun desr-rule->state (format state-name next-state)
  (cond
    ((null format)
     (error "Wrong format"))
    ((stringp format)
     (state-for :str format state-name next-state))
    ((eq :eps format)
     (state-for :eps format state-name next-state))
    ((symbolp format)
     (state-for :call format state-name next-state))
    ((listp format)
     (if (member (first format)
                 '(:lex :or :* :+))
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
      (dolist (rule grammar)
        (let ((state (gensym)))
          (@add-rule (first rule) state (rest (rest rule)))
          (rule->state rule state)))
      atn)))
