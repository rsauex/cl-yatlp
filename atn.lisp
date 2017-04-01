(defpackage #:atn
  (:use #:cl #:alexandria #:eager-future2)
  (:export #:make-atn

           #:rule
           #:state
           #:end-state
           #:call-state

           #:with-atn

           #:@add-state
           #:@state-type
           #:@add-rule
           #:@state-type
           #:@state-cond
           #:@state-nexts
           #:@state-call-to
           #:@state-end-type
           #:@state-end-options
           #:@get-rule
           #:@rule-state
           #:@rule-options
           #:@typep
           #:@rem-rule
           #:@rem-state
           #:@states
           #:@state-nexts-without-end
           #:@state-end-state
           #:delayed-rule

           #:with-visiting
           #:visit

           #:def-state-generic
           #:def-state-method

           #:atn->dot
           #:@atn->dot))

(in-package #:atn)

;;; Structure

(defstruct atn
  (table (make-hash-table))
  (rules (make-hash-table)))

;;; Rule

(defstruct rule
  state
  options)

;;; Possible states

(defclass state ()
  ((cond :accessor state-cond
         :initarg :cond
         :initform t)
   (nexts :accessor state-nexts
          :initarg :nexts
          :type list
          :initform nil)))

(defclass call-state (state)
  ((to :accessor call-to
       :initarg :call-to
       :type state)))

(defclass end-state (state)
  ((type :accessor end-type
         :initarg :type)
   (options :accessor end-options
           :initarg :options)))

;;; Tools

(defun apply-delta (atn delta)
  (maphash (lambda (id state)
             (if (eq :rem state)
                 (remhash id (atn-table atn))
                 (setf (gethash id (atn-table atn)) state)))
           (atn-table delta))
  (maphash (lambda (id rule)
             (if (eq :rem rule)
                 (remhash id (atn-rules atn))
                 (setf (gethash id (atn-rules atn)) rule)))
           (atn-rules delta)))

(defvar *atn* nil
  "Current ATN")

(defvar *delta* nil
  "Changes in current atn")

(defmacro with-atn (atn &body body)
  "Binds *STATE-TABLE* to STATE-TABLE"
  `(let ((atn::*atn* ,atn)
         (atn::*delta* (make-atn)))
     (multiple-value-prog1
         (progn ,@body)
       (apply-delta atn::*atn* atn::*delta*))))

(defun add-rule (atn rule state options)
  "Add rule into atn"
  (setf (gethash rule (atn-rules atn)) (make-rule :state state :options options)))

(defun @add-rule (rule state options)
  "Add rule into current atn"
  (if *atn*
      (add-rule *delta* rule state options)
      (error "@ functions must be used inside of WITH-ATN")))

(defun get-rule (atn rule)
  "Get state for rule with name RULE."
  (if-let (res (gethash rule (atn-rules atn)))
    res
    (error "No such rule: ~S" rule)))

(defun @get-rule (rule)
  "Get state for rule with name RULE."
  (if *atn*
      (or (gethash rule (atn-table *delta*))
          (get-rule *atn* rule))
      (error "@ functions must be used inside of WITH-ATN")))

(defun add-state (atn state type &rest args)
  "Add state named STATE of type TYPE into ATN.
ARGS are passed to the constructor of class TYPE"
  (setf (gethash state (atn-table atn)) 
        (apply #'make-instance type args)))

(defun @add-state (state type &rest args)
  "Add state named STATE of type TYPE into current atn
ARGS are passed to the constructor of class TYPE"
  (if *atn*
      (apply #'add-state *delta* state type args)
      (error "@ functions must be used inside of WITH-ATN")))

(defun get-state (atn state)
  "Get state from ATN. Error if STATE is not in ATN"
  (if-let (res (gethash state (atn-table atn)))
    res
    (error "No such state: ~S" state)))

(defun @get-state (state)
  "Get state from current atn.
Error if STATE is not in current atn"
  (if *atn*
      (or (gethash state (atn-table *delta*))
          (get-state *atn* state))
      (error "@ functions must be used inside of WITH-ATN")))

(defun @state-type (state)
  (type-of (@get-state state)))

(defun @typep (state type)
  (if-let (s (@get-state state))
    (typep s type)
    (error "No such state: ~S" state)))

(defun @rule-state (rule)
  "Get start states for rule in current atn"
  (rule-state (@get-rule rule)))

(define-setf-expander @rule-options (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (rule-state (@get-rule ,getter)) ,store) ,store)
              `(@state-cond ,getter)))))

(defun @rule-options (rule)
  "Get options for rule in current atn"
  (rule-options (@get-rule rule)))

(define-setf-expander @rule-options (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (rule-options (@get-rule ,getter)) ,store) ,store)
              `(@state-cond ,getter)))))

(defun @state-cond (state)
  "Get cond for specefied state from current atn"
  (state-cond (@get-state state)))

(define-setf-expander @state-cond (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (state-cond (@get-state ,getter)) ,store) ,store)
              `(@state-cond ,getter)))))

(defun @state-nexts (state)
  "Get nexts for specified state in current atn"
  (state-nexts (@get-state state)))

(define-setf-expander @state-nexts (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (state-nexts (@get-state ,getter)) ,store) ,store)
              `(@state-nexts ,getter)))))

(defun delayed-rule (name)
  (pcall (lambda () (rule-state (@get-rule name))) :lazy))

(defun @state-call-to (state)
  (or (touch (call-to (@get-state state)))
      (error "call-to cannot be NIL")))

(define-setf-expander @state-call-to (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (call-to (@get-state ,getter)) ,store) ,store)
              `(@state-nexts ,getter)))))

(defun @state-end-type (state)
  "Get type of end state in current atn"
  (end-type (@get-state state)))

(define-setf-expander @state-end-type (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (end-type (@get-state ,getter)) ,store) ,store)
              `(@state-nexts ,getter)))))

(defun @state-end-options (state)
  "Get options for end state in current atn"
  (end-options (@get-state state)))

(define-setf-expander @state-end-options (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (end-options (@get-state ,getter)) ,store) ,store)
              `(@state-nexts ,getter)))))

(defun rem-state (atn state)
  (remhash state (atn-table atn)))

(defun @rem-state (state)
  "Remove state from current atn"
  (if *atn*
      (setf (gethash state (atn-table *delta*)) :rem)
      (error "@ functions must be used inside of WITH-ATN")))

(defun rem-rule (atn rule)
  (remhash rule (atn-rules atn)))

(defun @rem-rule (rule)
  "Remove rule from current atn"
  (if *atn*
      (setf (gethash rule (atn-rules *delta*)) :rem)
      (error "@ functions must be used inside of WITH-ATN")))

(defun @states ()
  (if *atn*
      (union (hash-table-keys (atn-table *atn*))
             (remove-if (lambda (s) (eq :rem (@get-state s)))
                        (hash-table-keys (atn-table *delta*))))
      (error "@ functions must be used inside of WITH-ATN")))

(defun @state-end-state (state)
  (first (member-if (lambda (s) (@typep s 'end-state)) (@state-nexts state))))

(defun @state-nexts-without-end (state)
  (remove-if (lambda (s) (@typep s 'end-state)) (@state-nexts state)))

;;; Visiting

(defvar *visited* nil
  "List of already visited states")

(defmacro with-visiting (&body body)
  "Allows using of visit fn"
  `(let ((*visited*))
     ,@body))

(defun visit (state)
  (if (member state *visited*)
      state
      (progn
        (push state *visited*)
        nil)))

;;; Traversal

(defun @traverse-atn (fn)
  (mapc fn (@states)))

;;; Generic

(eval-when (:compile-toplevel :execute)
  (defmacro def-state-generic (fun-name lambda-list &body options)
    "Defines generic where first parameter must be state.
These generic functions must be called only within with-atn"
    (multiple-value-bind (required optional rest keys)
        (parse-ordinary-lambda-list lambda-list) 
      (let ((generic-name (symbolicate "%%" fun-name))
            (params-for-call (if rest
                                 `(,@required ,@(mapcar #'first optional) ,rest)
                                 `(,@required ,@(mapcar #'first optional) ,@(mappend #'first keys)))))
        `(progn
           (defgeneric ,generic-name ,(cons 'type lambda-list)
             ,@options)
           (defun ,fun-name ,lambda-list
             ,(if rest
                  `(apply #',generic-name ,@(cons `(@get-state ,(first required)) params-for-call))
                  `(,generic-name ,@(cons `(@get-state ,(first required)) params-for-call))))))))


  (defun parse-defmethod-args (args)
    (loop for sublist on args
          when (not (listp (first sublist)))
            collect (first sublist) into qualifiers
          else do (return-from parse-defmethod-args
                    (values qualifiers (first sublist) (rest sublist)))))

  (defmacro def-state-method (name &rest args)
    (multiple-value-bind (qualifiers params body)
        (parse-defmethod-args args)
      (let* ((generic-name (symbolicate "%%" name))
             (param (first params))
             (type (if (listp param)
                       (second param)
                       t))
             (arg-name (if (listp param)
                           (first param)
                           param)))
        `(defmethod ,generic-name ,@qualifiers ,(cons `(type ,type) (cons arg-name (rest params)))
           ,@body)))))

;;; Visual representation

(def-state-generic state->dot (state stream)
  (:documentation
   "Output state's representation in dot format into stream"))

(def-state-method state->dot ((state state) stream)
  (format stream "~A [label = \"~A\\n~A [~A]\"];~%~A -> {~{~A~^ ~}};~%"
          state state (type-of (@get-state state)) (@state-cond state) state (@state-nexts state)))

(def-state-method state->dot ((state end-state) stream)
  (format stream "~A [peripheries=2 label=\"~A\\n~A [~A]\\antype = ~A\"];~%"
          state state (@state-type state) (@state-cond state) (@state-end-type state)))

(defun atn->dot (atn &optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (with-atn atn
    (format stream "digraph g {~%")
    (@traverse-atn (rcurry #'state->dot stream))
    (format stream "}")))

(defun @atn->dot (&optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (format stream "digraph g {~%")
  (@traverse-atn (rcurry #'state->dot stream))
  (format stream "}"))
