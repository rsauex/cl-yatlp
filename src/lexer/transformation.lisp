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
                                   (list (copy-call (first s) nexts (gensym))
                                         (second s)))
                                 (@state-nexts state))))
  state-id)

(defun insert-calls ()
  (@traverse-atn
   (lambda (id)
     (@add-state id 'simple-state
                 :nexts `((,(copy-call (@state-call-to id) (@state-nexts id)) :eps))))
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
                                 (second next))
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
                (when (eq :eps (second next))
                  (%eps-closure (first next))))
              (@state-nexts state)))))

(def-state-method %eps-closure ((state end-state))
  (unless (visit state)
    (list state)))

(defun eps-closure (state)
  "Return a set of states which are accessable for the given one, consiring that 
the state itself is always accessable, without consuming any characters."
  (with-visiting
    (sort (%eps-closure state)
          #'string< :key #'symbol-name)))

(defun eps-closure-for-set (set)
  "Computes epsilon closure for each state in `set' and returns the set
which includes all of that closures."
  (sort (remove-duplicates
         (mappend #'eps-closure set))
        #'string< :key #'symbol-name))

(defun nexts-for (set cond)
  "Return an epsilon closure for the set of states which can be accessed
after states in `set' depending on the value of cond. Cond must not be :eps"
  (eps-closure-for-set
   (mappend (lambda (state)
              (mappend (lambda (next)
                         (and (not (eq :eps (second next)))
                              (cond-equal cond 
                                          (cond-intersection cond (second next)))
                              (list (first next))))
                       (@state-nexts state)))
            set)))

;; States sets

(defvar *sets* nil
  "Holds corrent table of registered state in form of [set -> state in DFA]")

(defun try-register-set (set &optional (name (gensym)))
  "Returns two values: the first one is always the name of the set
and the second one is T when the state wasn't registred before or NIL otherwise.
If the state is empty raise an error."
  (when (null set)
   (error "Empty state during NFA->DFA transformation"))
  (if-let (old-name (gethash set *sets*))
    old-name
    (values (setf (gethash set *sets*) name) t)))

(defun %nfa->dfa/set (set)
  "Tries to register a set and if it has already been registred just returns its name.
If the state is \"new\" then it creates a new state in DFA and computes next states
for it."
  (multiple-value-bind (set-name new?)
      (try-register-set set)
    (unless new?
      (return-from %nfa->dfa/set set-name))

    (@add-state set-name 'simple-state
                :nexts (mapcar (lambda (cond)
                                 (let ((nexts (nexts-for set cond)))
                                   (list (%nfa->dfa/set nexts) cond)))
                               (conds-for-set set)))
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
  (let ((ng-loops-starts nil))
    (maphash
     (lambda (set state)
       (when (member-if (rcurry #'@typep 'ng-loop-start) set)
         (push state ng-loops-starts)))
     *sets*)
    (maphash
     (lambda (set state)
       (let ((end-state (get-best-end set)))
         (when end-state
           (@add-state state 'end-state
                       :nexts (if (member-if (rcurry #'@typep 'ng-loop-end) set)
                                  (remove-if (compose (rcurry #'member ng-loops-starts) #'first)
                                             (@state-nexts state))
                                  (@state-nexts state))
                       :type (@state-end-type end-state)
                       :options (@state-end-options end-state)))))
     *sets*)))

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
                 (mapc (compose #'%visit #'first) (@state-nexts state)))))
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
