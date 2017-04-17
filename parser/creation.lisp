(defpackage #:parser-creation
  (:use #:cl #:alexandria #:atn #:parser-states)
  (:export #:grammar->atn))

(in-package #:parser-creation)

(defvar *format*)

(defgeneric state-for (rule-type format state-name next-state))

(defmethod state-for (rule-type format state-name next-state)
  (error "Wrong sub-rule format ~A" format))

(defmethod state-for ((rule-type (eql :call)) format state-name next-state)
  (push :_ *format*)
  (@add-state state-name 'rule-state :nexts (list next-state)
                                     :rule format))

(defmethod state-for ((rule-type (eql :str)) format state-name next-state)
  (push format *format*)
  (@add-state state-name 'str-state :nexts (list next-state)
                                    :str format))

(defmethod state-for ((rule-type (eql :eps)) format state-name next-state)
  (@add-state state-name 'eps-state :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :lex)) format state-name next-state)
  (push :lex *format*)
  (@add-state state-name 'lex-state :nexts (list next-state)
                                    :lex (second format)))

(defmethod state-for ((rule-type (eql :^)) format state-name next-state)
  (@add-state state-name 'mimic-state :nexts (list next-state)
                                      :rule (second format)))

(defmethod state-for ((rule-type (eql :seq)) format state-name next-state)
  (cond
    ((and (null (rest (rest format)))
          (member (second format) '(:{ :} :v :> :< :. :!)))
     (desr-rule->state (first format) state-name next-state)
     (desr-rule->state (second format) nil nil))
    ((null (rest format))
     (desr-rule->state (first format) state-name next-state))
    ((member (first format) '(:{ :} :v :> :< :. :!))
     (desr-rule->state (first format) nil nil)
     (state-for :seq (rest format) state-name next-state))
    (t
     (let ((seq-end (gensym)))
       (desr-rule->state (first format) state-name seq-end)
       (state-for :seq (rest format) seq-end next-state)))))

(defmethod state-for ((rule-type (eql :format)) format state-name next-state)
  (push format *format*))

(defun desr-rule->state (format state-name next-state)
  (cond
    ((null format)
     (error "Wrong format"))
    ((stringp format)
     (state-for :str format state-name next-state))
    ((eq :eps format)
     (state-for :eps format state-name next-state))
    ((member format '(:{ :} :v :> :< :. :!))
     (state-for :format format state-name next-state))
    ((symbolp format)
     (state-for :call format state-name next-state))
    ((listp format)
     (cond
       ((member (first format) '(:^ :lex))
        (state-for (first format) format state-name next-state))
       (t (state-for :seq format state-name next-state))))
    (t (error "Wrong rule format ~A" format))))

(defgeneric rule-for (rule-type format rule-name options))

(defmethod rule-for ((rule-type (eql ':or)) format rule-name options)
  (@add-rule rule-name 'or-rule
             :state (mapcar (lambda (f)
                              (let (*format*
                                    (alter (gensym))
                                    (end-sym (gensym)))
                                (desr-rule->state f alter end-sym)
                                (@add-state end-sym 'p-end-state :type rule-name
                                                               :format (reverse *format*))
                                alter))
                            (rest format))
             :options options))

(defmethod rule-for ((rule-type (eql ':*)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((*format* (list delim :*))
          (body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 's-loop-rule :state (list body-sym)
                                        :delim delim
                                        :options options)
      (desr-rule->state body body-sym end-sym)
      (@add-state end-sym 'p-end-state :type rule-name :format (reverse *format*)))))

(defmethod rule-for ((rule-type (eql ':+)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((*format* (list delim :+))
          (body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 'p-loop-rule :state (list body-sym)
                                        :delim delim
                                        :options options)
      (desr-rule->state body body-sym end-sym)
      (@add-state end-sym 'p-end-state :type rule-name :format (reverse *format*)))))

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
       (let (*format*
             (state-name (gensym))
             (end-name (gensym)))
         (@add-rule name 'simple-rule :state (list state-name)
                                      :options options)
         (desr-rule->state format state-name end-name)
         (@add-state end-name 'p-end-state :type name :format (reverse *format*)))))))

(defun grammar->atn (grammar)
  (let ((atn (make-atn))) 
    (with-atn atn 
      (dolist (rule grammar)
        (rule->state rule))
      (setf (@extra :start-rule) (first (first grammar)))
      atn)))
