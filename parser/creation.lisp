(defpackage #:parser-creation
  (:use #:cl #:alexandria #:atn #:parser-states)
  (:export #:grammar->atn))

(in-package #:parser-creation)

(defgeneric state-for (rule-type format state-name next-state))

(defmethod state-for (rule-type format state-name next-state)
  (error "Wrong sub-rule format ~A" format))

(defmethod state-for ((rule-type (eql :call)) format state-name next-state)
  (@add-state state-name 'rule-state :nexts (list next-state)
                                     :rule format))

(defmethod state-for ((rule-type (eql :str)) format state-name next-state)
  (@add-state state-name 'str-state :nexts (list next-state)
                                    :str format))

(defmethod state-for ((rule-type (eql :eps)) format state-name next-state)
  (@add-state state-name 'eps-state :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :lex)) format state-name next-state)
  (@add-state state-name 'lex-state :nexts (list next-state)
                                    :lex (second format)))

(defmethod state-for ((rule-type (eql :^)) format state-name next-state)
  (@add-state state-name 'mimic-state :nexts (list next-state)
                                      :rule (second format)))

(defmethod state-for ((rule-type (eql :seq)) format state-name next-state)
  (if (null (rest format))
      (desr-rule->state (first format) state-name next-state)
      (let ((seq-end (gensym)))
        (desr-rule->state (first format) state-name seq-end)
        (state-for :seq (rest format) seq-end next-state))))

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
     (if (member (first format) '(:^ :lex))
         (state-for (first format) format state-name next-state)
         (state-for :seq format state-name next-state)))
    (t (error "Wrong rule format ~A" format))))

(defgeneric rule-for (rule-type format rule-name options))

(defmethod rule-for ((rule-type (eql ':or)) format rule-name options)
  (@add-rule rule-name 'or-rule
             :state (mapcar (lambda (f)
                              (let ((alter (gensym))
                                    (end-sym (gensym)))
                                (@add-state end-sym 'end-state :type rule-name)
                                (desr-rule->state f alter end-sym)
                                alter))
                            (rest format))
             :options options))

(defmethod rule-for ((rule-type (eql ':*)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 's-loop-rule :state (list body-sym)
                                        :delim delim)
      (@add-state end-sym 'end-state :type rule-name)
      (desr-rule->state body body-sym end-sym))))

(defmethod rule-for ((rule-type (eql ':+)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 'p-loop-rule :state (list body-sym)
                                        :delim delim)
      (@add-state end-sym 'end-state :type rule-name)
      (desr-rule->state body body-sym end-sym))))

(defun rule->state (rule)
  (destructuring-bind (name format &rest options)
      rule
    (cond
      ((null format)
       (error "Wrong format"))
      ((and (listp format)
            (member (first format) '(:or :* :+)))
       (rule-for (first format) format name options))
      (t
       (let ((state-name (gensym))
             (end-name (gensym)))
         (@add-rule name 'simple-rule :state (list state-name)
                                      :options options)
         (@add-state end-name 'end-state :type name)
         (desr-rule->state format state-name end-name))))))

(defun grammar->atn (grammar)
  (let ((atn (make-atn))) 
    (with-atn atn 
      (dolist (rule grammar)
        (rule->state rule))
      atn)))
