(uiop:define-package #:cl-yatlp/src/parser/states
  (:use #:cl #:cl-yatlp/src/atn)
  (:export #:simple-state
           #:rule-state
           #:instruction-state
           #:push-state
           #:pop-state
           #:build-state
           #:start-list-state
           #:end-list-state
           #:add-to-list-state
           #:store-state
           #:end-state
           
           #:simple-rule
           #:or-rule
           #:loop-rule
           #:s-loop-rule
           #:p-loop-rule))

(in-package #:cl-yatlp/src/parser/states)

(defstate simple-state (state) ())

;;; TODO: is it needed
(defstate end-state (state)
  ((type :accessor state-end-type
         :initarg :type)
   (options :accessor state-end-options
            :initform nil
            :initarg :options)))

;;;; Intermediate states

(defstate rule-state (simple-state)
  ((rule :accessor state-rule
         :initarg :rule)))

;;;; Instruction states

(defstate instruction-state (simple-state) ())

(defstate push-state (instruction-state)
  ((descriptor :accessor state-push-descriptor
               :initarg :descriptor)))

(defstate pop-state (instructtion-state) ())

(defstate build-state (instruction-state)
  ((alternative :accessor state-build-alternative
                :initarg :alternative)))

(defstate start-list-state (instruction-state) ())
(defstate end-list-state (instruction-state) ())
(defstate add-to-list-state (instruction-state) ())

(defstate store-state (instruction-state) ()) ;; TODO: Rename!

;;;;;;;;;;;;;;;;;

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
