(uiop:define-package #:cl-yatlp/src/lexer/transformation
  (:use #:cl #:alexandria #:cl-yatlp/src/lexer/cond #:cl-yatlp/src/atn #:cl-yatlp/src/lexer/states #:cl-yatlp/src/lexer/creation)
  (:export #:nfa->dfa))

(in-package #:cl-yatlp/src/lexer/transformation)

;;; Call insertation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replaces every call state with the body of the corresponding fragment.
;; Example:
;;
;;  --> (:call frag1) -->
;;  frag1 : (:simple) --(#\a)--> (:simple) --(:eps)-> (:end-state)
;;
;; After call insertation:
;;  --> (:simple) --(:eps)--> (:simple) --(#\a)--> (:simple-state) --(:eps)--> (:simple-state) -->
;;

(defun copy-call (state nexts &optional (state-id (gensym)))
  (if (@typep state 'end-state)
      (@add-state state-id 'simple-state :nexts nexts)
      (@add-state state-id (@state-type state)
                  :nexts (mapcar (lambda (s)
                                   (make-next :state (copy-call (next-state s) nexts (gensym))
                                              :cond (next-cond s)))
                                 (@state-nexts state))))
  state-id)

(defun insert-calls ()
  (@traverse-atn
   (lambda (id)
     (@add-state id 'simple-state
                 :nexts `(,(make-next :state (copy-call (@state-call-to id) (@state-nexts id)) :cond :eps))))
   :filter (rcurry #'@typep 'call-state)))

;;; NFA -> DFA transformation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look at the presentation "Lexical analysis" by Harry H. Porter, 2005 for information.

;; Functions for working with conds

(defun conds-for-set (set)
  "Return a disjoint-set of conds for a set of states."
  (possible-values
   (remove-duplicates
    (remove :eps
            (mappend (lambda (state)
                       (mapcar (lambda (next)
                                 (next-cond next))
                               (@state-nexts state)))
                     set))
    :test #'cond-equal)))

;; Epsilon closure

(def-state-generic %eps-closure (state)
  (:documentation "Internal function for epsilon closure computation."))

(def-state-method %eps-closure ((state simple-state))
  (unless (visit state)
    (cons
     state
     (mappend (lambda (next)
                (when (eq :eps (next-cond next))
                  (%eps-closure (next-state next))))
              (@state-nexts state)))))

(def-state-method %eps-closure ((state end-state))
  (unless (visit state)
    (list state)))

(defun eps-closure (state)
  "Return a set of states which are accessable for the given one, consiring that 
the state itself is always accessable, without consuming any characters."
  (with-visiting
    (%eps-closure state)))

(defun eps-closure-for-set (set)
  "Computes epsilon closure for each state in `set' and returns the set
which includes all of that closures."
  (remove-duplicates
   (mappend #'eps-closure set)))

(defun nexts-for (set cond)
  "Return an epsilon closure for the set of states which can be accessed
after states in `set' depending on the value of cond. Cond must not be :eps"
  (unless (and (some (rcurry #'@typep 'end-state) set)
               (some (compose (curry #'some (compose (curry #'eq :low) #'next-priority)) #'@state-nexts) set))
    (let ((next-set nil))
      (loop :for state :in set :do
        (loop :for next :in (@state-nexts state)
              :unless (or (eq :eps (next-cond next))
                          (not (cond-equal cond (cond-intersection cond (next-cond next)))))
                :do (push (next-state next) next-set)))
      (eps-closure-for-set (remove-duplicates next-set)))))

(defun nexts-for-set (set)
  "Compute next sets for the given set for all the conditions."
  (loop :for cond :in (conds-for-set set)
        :for nexts := (nexts-for set cond)
        :when nexts
          :collect (make-next :state (%nfa->dfa/set nexts) :cond cond)))

;; States sets

(defvar *sets* nil
  "Holds corrent table of registered state in form of [set -> state in DFA]")

(defun try-register-set (set)
  "Returns two values: the first one is always the name of the set
and the second one is T when the state wasn't registred before or NIL otherwise.
If the state is empty raise an error."
  (when (null set)
   (error "Empty state during NFA->DFA transformation"))
  (if-let (old-name (gethash set *sets*))
    old-name
    (values (setf (gethash set *sets*) (gensym)) t)))

(defun %nfa->dfa/set (set)
  "Tries to register a set and if it has already been registred just returns its name.
If the state is \"new\" then it creates a new state in DFA and computes next states
for it."
  (multiple-value-bind (set-name new?)
      (try-register-set set)
    (when new?
      (@add-state set-name 'simple-state :nexts (nexts-for-set set)))
    set-name))

;; End computation

(defun get-best-end (states)
  "According to the order of the rules in the grammar returns the best end state
from the given set of states. Return NIL if there are no end states in `states'."
  (let* ((ends (remove-if-not (rcurry #'@typep 'end-state) states))
         (name (first (member-if (rcurry #'member ends :key #'@state-end-type) (@extra :order)))))
    (first (member-if (compose (curry #'eq name) #'@state-end-type) ends))))

(defun compute-ends ()
  "Finds the states in DFA for which the corresponding set of states in NFA
contains end state and makes it an end state with the type of the best end state
in the corresponding set of NFA states."
  (maphash
   (lambda (set state)
     (let ((end-state (get-best-end set)))
       (when end-state
         (@add-state state 'end-state
                     :nexts (@state-nexts state)
                     :type (@state-end-type end-state)
                     :options (@state-end-options end-state)))))
   *sets*))

;; nfa->dfa

(defun %nfa->dfa ()
  "Entry point to NFA->DFA transformation"
  (let* ((*sets* (make-hash-table :test #'equalp))
         (new-start (%nfa->dfa/set (eps-closure (@extra :start)))))
    (compute-ends)
    (setf (@extra :start) new-start)))

;;; Removing unreachable states ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-unreachable-states (start)
  "Removes all the states which are unreachable from the `start' state."
  (with-visiting
    (labels ((%visit (state)
               (unless (visit state)
                 (mapc (compose #'%visit #'next-state) (@state-nexts state)))))
      (%visit start)
      (mapc (lambda (s)
              (unless (visit s)
                (@rem-state s)))
            (@states)))))

;;; Main entry point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nfa->dfa (nfa)
  "Transforms the given nfa into dfa, which is the returning value.
The original nfa is modified."
  (with-atn nfa
    (insert-calls)
    (%nfa->dfa)
    (remove-unreachable-states (@extra :start)))
  nfa)
