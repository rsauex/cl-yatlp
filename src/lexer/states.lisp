(uiop:define-package #:cl-yatlp/src/lexer/states
  (:use #:cl #:cl-yatlp/src/atn)
  (:export #:simple-state
           #:ng-loop-start
           #:ng-loop-end
           #:end-state))

(in-package #:cl-yatlp/src/lexer/states)

(defstate simple-state (state) ()
  (:documentation "Simple state in lexer FA"))

(defstate end-state (state)
  ((type :accessor state-end-type
         :initarg :type)
   (options :accessor state-end-options
            :initform nil
            :initarg :options))
  (:documentation "End state in lexer FA"))

;;; Lexer states to dot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-state-method state->dot ((state simple-state) stream)
  (format stream "  ~A [label = \"~A\\n~A [~A]\"];~%"
          state state (type-of (@get-state state)) (@state-cond state))
  (dolist (x (@state-nexts state))
    (if (eq :low (next-priority x))
        (format stream "  ~A -> ~A [label=\"~A [:low]\"]~%"
                state (next-state x) (next-cond x))
        (format stream "  ~A -> ~A [label=\"~A\"]~%"
                state (next-state x) (next-cond x)))))

(def-state-method state->dot ((state end-state) stream)
  (format stream "  ~A [peripheries=2 label=\"~A\\n~A [~A]\\ntype = ~A\"];~%"
          state state (@state-type state) (@state-cond state) (@state-end-type state))
  (dolist (x (@state-nexts state))
    (format stream "  ~A -> ~A [label=\"~A\"]~%"
            state (next-state x) (next-cond x))))
