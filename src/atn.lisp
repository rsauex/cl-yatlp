(uiop:define-package #:cl-yatlp/src/atn
  (:use #:cl #:alexandria)
  (:export #:make-atn

           #:defstate
           #:defrule

           #:rule
           #:state
           #:call-state

           #:with-atn

           #:@extra

           #:@get-state
           #:@add-state
           #:@rem-state
           #:@state-type
           #:@states
           #:@state-nexts-without-end

           #:@get-rule
           #:@add-rule
           #:@rem-rule
           #:@rules

           #:@typep
           #:@same-ids?

           #:delayed-rule

           #:make-next
           #:next-state
           #:next-cond
           #:next-priority
           
           #:with-visiting
           #:visit

           #:@traverse-atn

           #:def-state-generic
           #:def-state-method
           #:def-rule-generic
           #:def-rule-method

           #:state->dot
           #:atn->dot
           #:@atn->dot))


(in-package #:cl-yatlp/src/atn)

;;; States

(eval-when (:compile-toplevel :load-toplevel)
  (defun make-et-accessor (slot element-getter)
    (let* ((raw-accessor (or (getf (rest slot) :accessor) (first slot)))
           (et-accessor (symbolicate "@" raw-accessor)))
      `(progn
         (defun ,et-accessor (id)
           (,raw-accessor (,element-getter (force-id id))))
         (defsetf ,et-accessor (store) (x)
           `(setf (,',raw-accessor (,',element-getter (force-id ,store))) ,x))
         (export ',et-accessor)))))

(defmacro defrule (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(mapcar (rcurry #'make-et-accessor '@get-rule) direct-slots)))

(defmacro defstate (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(mapcar (rcurry #'make-et-accessor '@get-state) direct-slots)))

;;; 

(defrule rule ()
  ((state :accessor rule-state
          :initarg :state)
   (options :accessor rule-options
            :initarg :options)))

;;;

(defstate state ()
  ((cond :accessor state-cond
         :initarg :cond
         :initform t)
   (nexts :accessor state-nexts
          :initarg :nexts
          :type list
          :initform nil)))

(defstate call-state (state)
  ((to :accessor state-call-to
       :initarg :call-to)))

(defstruct next
  state
  (cond :eps)
  (priority :normal))

;;; Structure

(defstruct atn
  (table (make-hash-table))
  (rules (make-hash-table))
  (extra (make-hash-table)))

;;; Delayed id

(defstruct delayed-id
  (computed nil)
  (value nil))

(defun force-id (id)
  (if (delayed-id-p id)
      (if (delayed-id-computed id)
          (delayed-id-value id)
          (progn
            (setf (delayed-id-computed id) t)
            (setf (delayed-id-value id)
                  (funcall (delayed-id-value id)))))
      id))

;;; Tools

(defvar *atn* nil
  "Current ATN")

(defvar *delta* nil
  "Changes in current atn")

(defun apply-delta (atn delta)
  (labels ((%process-table (getter)
             (maphash (lambda (id state)
                        (if (eq :rem state)
                            (remhash id (funcall getter atn))
                            (setf (gethash id (funcall getter atn)) state)))
                      (funcall getter delta))))
    (%process-table #'atn-table)
    (%process-table #'atn-rules)))

(defmacro with-atn (atn &body body)
  "Binds *STATE-TABLE* to STATE-TABLE"
  `(let ((cl-yatlp/src/atn::*atn* ,atn)
         (cl-yatlp/src/atn::*delta* (make-atn)))
     (multiple-value-prog1
         (progn ,@body)
       (apply-delta cl-yatlp/src/atn::*atn* cl-yatlp/src/atn::*delta*))))

(defun @extra (name)
  (gethash name (atn-extra *atn*)))

(defsetf @extra (store) (x)
  `(setf (gethash ,store (atn-extra cl-yatlp/src/atn::*atn*)) ,x))

(defun @state-nexts-without-end (state)
  (remove-if (rcurry #'@typep 'end-state) (@state-nexts state)))

(defun delayed-rule (name)
  (make-delayed-id :value (lambda () (rule-state (@get-rule name)))))

(defun add-rule (atn rule type &rest args)
  "Add rule into atn"
  (setf (gethash rule (atn-rules atn)) (apply #'make-instance type args)))

(defun @add-rule (rule type &rest args)
  "Add rule into current atn"
  (apply #'add-rule *delta* rule type args))

(defun get-rule (atn rule)
  "Get state for rule with name RULE."
  (if-let (res (gethash (force-id rule) (atn-rules atn)))
    res
    (error "No such rule: ~S" rule)))

(defun @get-rule (rule)
  "Get state for rule with name RULE."
  (or (when-let (r (gethash (force-id rule) (atn-rules *delta*)))
        (if (eq r :rem)
            (error "No such rule: ~A" rule)
            r))
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
  (if-let (res (gethash (force-id state) (atn-table atn)))
    res
    (error "No such state: ~S" state)))

(defun @get-state (state)
  "Get state from current atn.
Error if STATE is not in current atn"
  (or (when-let (s (gethash (force-id state) (atn-table *delta*)))
        (if (eq s :rem)
            (error "No such state: ~A" state)
            s))
      (get-state *atn* (force-id state))))

(defun @state-type (state)
  "Returns type of the given state"
  (type-of (@get-state state)))

(defun @typep (state type)
  "Returns t when `state' is of type `type'.
nil otherwise."
  (typep (@get-state state) type))

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
  (union (remove-if (lambda (s) (eq :rem (gethash s (atn-table *delta*))))
                    (hash-table-keys (atn-table *atn*)))
         (remove-if (lambda (s) (eq :rem (gethash s (atn-table *delta*))))
                    (hash-table-keys (atn-table *delta*)))))

(defun @rules ()
  "Get all the rules from current atn"
  (union (remove-if (lambda (s) (eq :rem (gethash s (atn-rules *delta*))))
                    (hash-table-keys (atn-rules *atn*)))
         (remove-if (lambda (s) (eq :rem (gethash s (atn-rules *delta*))))
                    (hash-table-keys (atn-rules *delta*)))))

(defun @same-ids? (id1 id2)
  (eq (force-id id1) (force-id id2)))

;;; Visiting

(defvar *visited* nil
  "List of already visited states")

(defmacro with-visiting (&body body)
  "Allows using of visit fn"
  `(let (cl-yatlp/src/atn::*visited*)
     ,@body))

(defun visit (state)
  (or (first (member state *visited*))
      (not (push state *visited*))))

;;; Traversal

(defun @traverse-atn (fn &key filter)
  (mapc (lambda (s) 
          (when (or (null filter)
                    (funcall filter s))
            (funcall fn s)))
        (@states)))

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
      (let* ((generic-name (intern (symbol-name (symbolicate "%%" name)) (symbol-package name)))
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

(defun @atn->dot (&optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (format stream "digraph g {~%")
  (@traverse-atn (rcurry #'state->dot stream))
  (format stream "}"))

(defun atn->dot (atn &optional (stream *standard-output*))
  "Output ATN into stream in dot format"
  (with-atn atn
    (@atn->dot stream)))
