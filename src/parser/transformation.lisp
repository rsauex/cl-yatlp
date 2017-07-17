(defpackage #:cl-yatlp/parser-transformation
  (:use #:cl #:alexandria #:cl-yatlp/atn #:cl-yatlp/parser-states
        #:cl-yatlp/transformations/mimic
        #:cl-yatlp/transformations/opt-rule)
  (:export #:first-for-rule
           #:first-for-state
           #:follow-for-rule
           #:follow-for-state
           #:transform
           #:get-rule-for-state))

(in-package #:cl-yatlp/parser-transformation)

(def-state-generic first-for-state (state))

(def-state-method first-for-state ((state rule-state))
  (let ((rule-first (first-for-rule (@state-rule state))))
    (if (member :eps rule-first)
        (if (member-if (rcurry #'@typep 'end-state) (@state-nexts state))
            (if (= 1 (length (@state-nexts state)))
                rule-first
                (remove-duplicates
                 (append (mappend #'first-for-state (remove-if (rcurry #'@typep 'end-state)
                                                               (@state-nexts state)))
                         rule-first)))
            (remove-duplicates
             (append (mappend #'first-for-state (remove-if (rcurry #'@typep 'end-state)
                                                           (@state-nexts state)))
                     (remove :eps rule-first))))
        rule-first)))

(def-state-method first-for-state ((state lex-state))
  (list (list :lex (@state-lex state))))

(def-state-method first-for-state ((state str-state))
  (list (list :str (@state-str state))))

(def-state-method first-for-state ((state eps-state))
  (list :eps))

(def-state-method first-for-state ((state mimic-state))
  (first-for-rule (@state-mimic-rule state)))

(defun first-for-rule (rule)
  (mappend #'first-for-state (@rule-state rule)))

;;;;;;;;

(def-state-generic follow-for-state (state))

(def-state-method follow-for-state ((state rule-state))
  (follow-for-rule (@state-rule state)))

;; (def-state-method follow-for-state ((state mimic-state))
;;   (follow-for-rule (@state-mimic-rule state)))

(def-state-method follow-for-state ((state eps-state))
  (follow-for-rule (get-rule-for-state state)))

(def-state-method follow-for-state ((state simple-state))
  (loop for s in (@state-nexts state)
        appending (if (@typep s 'end-state)
                      (follow-for-rule (get-rule-for-state state))
                      (let ((res (first-for-state s)))
                        (if (member :eps res)
                            (append (remove :eps res)
                                    (follow-for-state s))
                            res)))))

(defun rule-has-state? (rule state)
  (labels ((%has? (s)
             (or (eq s state)
                 (member-if #'%has? (@state-nexts s)))))
     (member-if #'%has? (@rule-state rule))))

(defun get-rule-for-state (state)
  (first (member-if (rcurry #'rule-has-state? state) (@rules))))

(defun follow-for-rule (rule)
  (loop for s in (@states)
        when (or (and (@typep s 'rule-state)
                      (eq rule (@state-rule s)))
                 (and (@typep s 'mimic-state)
                      (eq rule (@state-mimic-rule s))))
        append (append (when (member-if (rcurry #'@typep 'end-state) (@state-nexts s))
                         (let ((rl (get-rule-for-state s)))
                           (unless (eq rl rule)
                             (follow-for-rule rl))))
                       (mappend #'first-for-state (@state-nexts-without-end s)))
        into res
        finally (return (remove-duplicates (remove :eps res) :test #'equal))))

;;;;;;;;

(defun uniquify-names (states)
  (let ((suffix 1))
    (labels ((%uniquify (state)
               (if (@typep state 'end-state)
                   (progn
                     (unless (= suffix 1)
                         (setf (@state-end-type state) (symbolicate (@state-end-type state)
                                                                    (format nil "-~A" suffix))))
                     (incf suffix))
                   (mapc #'%uniquify (@state-nexts state)))))
      (mapc #'%uniquify states))))

(def-rule-generic transform-rule (rule))

(def-rule-method transform-rule ((rule rule))
  (declare (ignore rule)))

(def-rule-method transform-rule ((rule or-rule))
  (let ((rules (make-hash-table))
        (lexs (make-hash-table))
        (strs (make-hash-table))
        (has-eps (member-if (rcurry #'@typep 'eps-state) (@rule-state rule))))
    (loop for s in (@rule-state rule)
          if (@typep s 'rule-state)
            do (push s (gethash (@state-rule s) rules))
          if (@typep s 'mimic-state)
            do (push s (gethash (@state-mimic-rule s) rules))
          if (@typep s 'lex-state)
            do (push s (gethash (@state-lex s) lexs))
          if (@typep s 'str-state)
            do (push s (gethash (@state-str s) strs)))
    (setf (@rule-state rule) nil)
    (dolist (states (hash-table-values rules))
      (if (= 1 (length states))
          (push (first states) (@rule-state rule))
          (let ((state (or (first (member-if (rcurry #'@typep 'mimic-state) states))
                           (first states))))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (@rule-state rule)))))
    (dolist (states (hash-table-values lexs))
      (if (= 1 (length states))
          (push (first states) (@rule-state rule))
          (let ((state (first states)))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (@rule-state rule)))))
    (dolist (states (hash-table-values strs))
      (if (= 1 (length states))
          (push (first states) (@rule-state rule))
          (let ((state (first states)))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (@rule-state rule)))))
    (if has-eps
        (push (first has-eps) (@rule-state rule)))
    (uniquify-names (@rule-state rule))))

(defun transform (atn)
  (with-atn atn
    (add-opt-rules)
    (mapc #'transform-rule (@rules))
    (add-mimics)
    atn))

;;;;;;;;

