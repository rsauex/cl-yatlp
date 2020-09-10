(uiop:define-package #:cl-yatlp/src/parser/creation
  (:use #:cl #:alexandria #:cl-yatlp/src/atn #:cl-yatlp/src/parser/states)
  (:export #:grammar->atn

           #:->
           #:-->
           #:*
           #:+))

(in-package #:cl-yatlp/src/parser/creation)

(defgeneric state-for (rule-type form next-state))

(defmacro def-state-for (type vars &rest body)
  "Macro for defining `state-for' methods. Each value in `vars' must be a symbol,
which will be \"let'ed\" to gensyms."
  `(defmethod state-for ((rule-type (eql ,type)) form next-state)
     (let ,(mapcar (rcurry #'list '(gensym)) vars)
       ,@body)))

(def-state-for :call (state-id)
  (@add-state state-id 'rule-state :nexts (list next-state)
                                   :rule form)
  (make-next :state state-id :cond :eps))

(def-state-for :str (state-id)
  (@add-state state-id 'simple-state :nexts (list next-state))
  (make-next :state state-id :cond (list :str form)))

(def-state-for :lex (state-id)
  (@add-state state-id 'simple-state :nexts (list next-state))
  (make-next :state state-id :cond (list :lex (second form))))

(def-state-for :eps ()
  next-state)

(def-state-for :seq ()
  (cond
    ((null (rest form))
     (desr-rule->state (first form) next-state))
    (t
     (desr-rule->state (first form) (state-for :seq (rest form) next-state)))))

(defun desr-rule->state (form next-state)
  (cond
    ((null form)
     (error "Wrong format"))
    ((stringp form)
     (state-for :str form next-state))
    ((eq :eps form)
     (state-for :eps form  next-state))
    ((symbolp form)
     (state-for :call form next-state))
    ((listp form)
     (cond
       ((member (first form) '(:lex))
        (state-for (first form) form next-state))
       (t (state-for :seq form next-state))))
    (t (error "Wrong rule format ~A" form))))

(defgeneric rule-for (rule-type format rule-name options))

(defmethod rule-for ((rule-type (eql ':or)) format rule-name options)
  (setf (gethash rule-name (@extra :rules-map))
        (let ((start-state (gensym)))
          (@add-state
           start-state
           'simple-state
           :nexts (mapcar (lambda (f)
                            (let ((build-state-id (gensym)))
                              (@add-state 
                               build-state-id
                               'build-state :alternative (gensym)
                                            :nexts '())
                              (desr-rule->state f (make-next :state build-state-id :cond :eps))))
                          format)))))

(defmethod rule-for ((rule-type (eql :*)) format rule-name options)
  (destructuring-bind (delim &rest body)
      format
    (let* ((start-sym (gensym))
           (add-to-list-sym (gensym))
           (end-sym (gensym))
           (body-next (desr-rule->state body (make-next :state add-to-list-sym :cond :eps)))
           (delimiter-next (desr-rule->state delim body-next)))
      (setf (gethash rule-name (@extra :rules-map))
            start-sym)
      (@add-state start-sym 'start-list-state :nexts (list body-next (make-next :state end-sym :cond :eps)))
      (@add-state add-to-list-sym 'add-to-list-state :nexts (list delimiter-next (make-next :state end-sym :cond :eps)))
      (@add-state end-sym 'build-state :alternative rule-name :nexts '()))))

(defmethod rule-for ((rule-type (eql :+)) format rule-name options)
  (destructuring-bind (delim &rest body)
      format
    (let* ((start-sym (gensym))
           (add-to-list-sym (gensym))
           (end-sym (gensym))
           (body-next (desr-rule->state body (make-next :state add-to-list-sym :cond :eps)))
           (delimiter-next (desr-rule->state delim body-next)))
      (setf (gethash rule-name (@extra :rules-map))
            start-sym)
      (@add-state start-sym 'start-list-state :nexts (list body-next))
      (@add-state add-to-list-sym 'add-to-list-state :nexts (list delimiter-next (make-next :state end-sym :cond :eps)))
      (@add-state end-sym 'build-state :alternative rule-name :nexts '()))))

(defun parse-rule (rule)
  "Parses rule into name alternatives and options.
Each rule has the following format (<name> {-> ...} [:options ...]"
  (destructuring-bind (name type &rest body) rule
    (labels ((%till (list test)
               (loop for x on list
                     unless (funcall test (first x))
                       collect (first x) into res
                     else
                       do (return (values res x))
                     finally (return res)))
             (%parse-body (body &optional acc)
               (if body
                   (ecase (first body)
                     (->
                      (multiple-value-bind (alternative rest)
                          (%till (rest body) (lambda (x) (member x '(:options -> -->))))
                        (%parse-body rest (cons alternative acc))))
                     (-->
                      (multiple-value-bind (alternative rest)
                          (%till (rest body) (lambda (x) (member x '(:options -> -->))))
                        (%parse-body rest (cons (cons :^ alternative) acc))))
                     (:options
                      (values (reverse acc) (rest body))))
                   (reverse acc))))
      (ecase type
        ((-> -->)
         (multiple-value-bind (alternatives options)
             (%parse-body (cons type body))
           (values name alternatives options :or)))
        ((:+ :*)
         (destructuring-bind (delim rule &rest options) body
           (values name (list delim rule) (rest options) type)))
        (:lex
         (destructuring-bind (regex &rest options) body
           (values name regex (rest options) type)))))))

(defun rule->state (rule)
  (multiple-value-bind (name alternatives options type)
      (parse-rule rule)
    (rule-for type alternatives name options)))

(defun grammar->atn (grammar)
  "Transformate the given grammar into ATN"
  (let ((atn (make-atn))) 
    (with-atn atn
      (setf (@extra :rules-map) (make-hash-table))
      (mapc #'rule->state grammar)
      (setf (@extra :start-rule) (first (first grammar)))
      atn)))
