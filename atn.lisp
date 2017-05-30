(defpackage #:atn
  (:use #:cl #:alexandria)
  (:export #:make-atn

           #:defstate
           #:defrule

           #:rule
           #:state
           #:end-state
           #:call-state

           #:with-atn

           #:@extra

           #:@get-state
           #:@add-state
           #:@rem-state
           #:@states
           #:@state-type
           #:@state-call-to

           #:@get-rule
           #:@add-rule
           #:@rem-rule
           #:@rules

           #:@typep
           
           #:@state-nexts-without-end
           #:delayed-rule

           #:with-visiting
           #:visit

           #:@traverse-atn

           #:def-state-generic
           #:def-state-method
           #:def-rule-generic
           #:def-rule-method

           #:atn->dot
           #:@atn->dot))

(in-package #:atn)

;;; Structure

(defstruct atn
  (table (make-hash-table))
  (rules (make-hash-table))
  (extra (make-hash-table)))

;;; Rule

(eval-when (:compile-toplevel :load-toplevel)
  (defun make-et-accessor (raw-accessor element-getter)
    (let ((et-accessor (symbolicate "@" raw-accessor)))
      `(progn
         (defun ,et-accessor (rule)
           (,raw-accessor (,element-getter rule)))
         (export ',et-accessor)
         (define-setf-expander ,et-accessor (x &environment env)
           (multiple-value-bind (dummies vals newval setter getter)
               (get-setf-expansion x env)
             (declare (ignorable newval setter))
             (let ((store (gensym)))
               (values dummies
                       vals
                       `(,store)
                       `(progn (setf (,',raw-accessor (,',element-getter ,getter)) ,store) ,store)
                       `(,',et-accessor ,getter)))))))))

(defmacro defrule (name direct-superclasses direct-slots &rest options)
  (let ((accessors (mapcar (lambda (slot)
                             (getf (rest slot) :accessor))
                           direct-slots)))
    `(progn
       (defclass ,name ,direct-superclasses
         ,direct-slots
         ,@options)
       ,@(mapcar (rcurry #'make-et-accessor '@get-rule) accessors))))

(defrule rule ()
  ((state :accessor rule-state
          :initarg :state)
   (options :accessor rule-options
            :initarg :options)))

;;; Possible states

(defmacro defstate (name direct-superclasses direct-slots &rest options)
  (let ((accessors (mapcar (lambda (slot)
                             (getf (rest slot) :accessor))
                           direct-slots)))
    `(progn
       (defclass ,name ,direct-superclasses
         ,direct-slots
         ,@options)
       ,@(mapcar (rcurry #'make-et-accessor '@get-state) accessors))))

(defstate state ()
  ((cond :accessor state-cond
         :initarg :cond
         :initform t)
   (nexts :accessor state-nexts
          :initarg :nexts
          :type list
          :initform nil)))

(defstate call-state (state)
  ((to :accessor call-to
       :initarg :call-to
       :type state)))

(defstate end-state (state)
  ((type :accessor state-end-type
         :initarg :type)
   (options :accessor state-end-options
           :initarg :options)))

;;; Tools

(defun @state-nexts-without-end (state)
  (remove-if (rcurry #'@typep 'end-state) (@state-nexts state)))

(defun delayed-rule (name)
  (lambda () (rule-state (@get-rule name))))

(defun @state-call-to (state)
  (or (let ((val (call-to (@get-state state))))
        (if (functionp val)
            (setf (call-to (@get-state state)) (funcall val))
            val))
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

(defun @extra (name)
  (gethash name (atn-extra *atn*)))

(define-setf-expander @extra (x &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion x env)
    (declare (ignorable newval setter))
    (let ((store (gensym)))
      (values dummies
              vals
              `(,store)
              `(progn (setf (gethash ,getter (atn-extra atn::*atn*)) ,store) ,store)
              `(@state-nexts ,getter)))))


(defun add-rule (atn rule type &rest args)
  "Add rule into atn"
  (setf (gethash rule (atn-rules atn)) (apply #'make-instance type args)))

(defun @add-rule (rule type &rest args)
  "Add rule into current atn"
  (apply #'add-rule *delta* rule type args))

(defun get-rule (atn rule)
  "Get state for rule with name RULE."
  (if-let (res (gethash rule (atn-rules atn)))
    res
    (error "No such rule: ~S" rule)))

(defun @get-rule (rule)
  "Get state for rule with name RULE."
  (or (gethash rule (atn-table *delta*))
      (get-rule *atn* rule)))

(defun add-state (atn state type &rest args)
  "Add state named STATE of type TYPE into ATN.
ARGS are passed to the constructor of class TYPE"
  (setf (gethash state (atn-table atn)) 
        (apply #'make-instance type args)))

(defun @add-state (state type &rest args)
  "Add state named STATE of type TYPE into current atn
ARGS are passed to the constructor of class TYPE"
  (apply #'add-state *delta* state type args))

(defun get-state (atn state)
  "Get state from ATN. Error if STATE is not in ATN"
  (if-let (res (gethash state (atn-table atn)))
    res
    (error "No such state: ~S" state)))

(defun @get-state (state)
  "Get state from current atn.
Error if STATE is not in current atn"
  (or (gethash state (atn-table *delta*))
      (get-state *atn* state)))

(defun @state-type (state)
  "Returns type of the given state"
  (type-of (@get-state state)))

(defun @typep (state type)
  "Returns t when `state' is of type `type'.
nil otherwise."
  (if-let (s (@get-state state))
    (typep s type)
    (error "No such state: ~S" state)))

(defun rem-state (atn state)
  (remhash state (atn-table atn)))

(defun @rem-state (state)
  "Remove state from current atn"
  (setf (gethash state (atn-table *delta*)) :rem))

(defun rem-rule (atn rule)
  (remhash rule (atn-rules atn)))

(defun @rem-rule (rule)
  "Remove rule from current atn"
  (setf (gethash rule (atn-rules *delta*)) :rem))

(defun @states ()
  "Get all the states from current atn"
  (union (hash-table-keys (atn-table *atn*))
         (remove-if (lambda (s) (eq :rem (@get-state s)))
                    (hash-table-keys (atn-table *delta*)))))

(defun @rules ()
  "Get all the rules from current atn"
  (union (hash-table-keys (atn-rules *atn*))
         (remove-if (lambda (s) (eq :rem (@get-rule s)))
                    (hash-table-keys (atn-rules *delta*)))))

;;; Visiting

(defvar *visited* nil
  "List of already visited states")

(defmacro with-visiting (&body body)
  "Allows using of visit fn"
  `(let (atn::*visited*)
     ,@body))

(defun visit (state)
  (or (first (member state *visited*))
      (not (push state *visited*))))

;;; Traversal

(defun @traverse-atn (fn)
  (mapc fn (@states)))

;;; Generic

(eval-when (:compile-toplevel :load-toplevel)
  (defun %def-%-generic (fun-name lambda-list options getter)
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
                  `(apply #',generic-name ,@(cons `(,getter ,(first required)) params-for-call))
                  `(,generic-name ,@(cons `(,getter ,(first required)) params-for-call))))))))

  (defun parse-defmethod-args (args)
    (loop for sublist on args
          when (not (listp (first sublist)))
            collect (first sublist) into qualifiers
          else do (return-from parse-defmethod-args
                    (values qualifiers (first sublist) (rest sublist)))))

  (defun %def-%-method (name args)
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
           ,@body))))

  (defmacro def-state-generic (fun-name lambda-list &body options)
    "Defines generic where first parameter must be state.
These generic functions must be called only within with-atn"
    (%def-%-generic fun-name lambda-list options '@get-state))

  (defmacro def-state-method (name &rest args)
    (%def-%-method name args))

  (defmacro def-rule-generic (fun-name lambda-list &body options)
    "Defines generic where first parameter must be state.
These generic functions must be called only within with-atn"
    (%def-%-generic fun-name lambda-list options '@get-rule))

  (defmacro def-rule-method (name &rest args)
    (%def-%-method name args)))

;;; Visual representation

(def-state-generic state->dot (state stream)
  (:documentation
   "Output state's representation in dot format into stream"))

(def-state-method state->dot ((state state) stream)
  (format stream "~A [label = \"~A\\n~A [~A]\"];~%~A -> {~{~A~^ ~}};~%"
          state state (type-of (@get-state state)) (@state-cond state) state (@state-nexts state)))

(def-state-method state->dot ((state end-state) stream)
  (format stream "~A [peripheries=2 label=\"~A\\n~A [~A]\\ntype = ~A\"];~%"
          state state (@state-type state) (@state-cond state) (@state-end-type state)))

(defun @atn->dot (&optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (format stream "digraph g {~%")
  (@traverse-atn (rcurry #'state->dot stream))
  (format stream "}"))

(defun atn->dot (atn &optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (with-atn atn
    (@atn->dot stream)))
