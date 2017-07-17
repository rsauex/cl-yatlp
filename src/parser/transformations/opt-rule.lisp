(defpackage #:cl-yatlp/transformations/opt-rule
  (:use #:cl #:alexandria #:cl-yatlp/atn #:cl-yatlp/parser-states)
  (:export #:add-opt-rules))

(in-package #:cl-yatlp/transformations/opt-rule)

(defun opt-rule? (rule)
  (let ((name (symbol-name rule)))
    (char= #\? (aref name (1- (length name))))))

(defun add-opt-rule (name)
  (let* ((mimic (gensym))
         (mimic-end (gensym))
         (eps (gensym))
         (eps-end (gensym))
         (rule-name (symbol-name name))
         (real-rule (subseq rule-name 0 (1- (length rule-name))))
         (real-rule-sym (intern real-rule (symbol-package name))))
    (@add-state mimic 'mimic-state :nexts (list mimic-end)
                                   :rule real-rule-sym)
    (@add-state mimic-end 'end-state :type name
                                     :options nil)
    (@add-state eps 'eps-state :nexts (list eps-end))
    (@add-state eps-end 'end-state :type name
                                   :options nil)
    (@add-rule name 'or-rule :state (list eps mimic)
                             :options nil)))

(defun add-opt-rules ()
  (@traverse-atn
   (lambda (state)
     (cond
       ((@typep state 'rule-state)
        (when (opt-rule? (@state-rule state))
          (add-opt-rule (@state-rule state))))
       ((@typep state 'mimic-state)
        (when (opt-rule? (@state-mimic-rule state))
          (add-opt-rule (@state-mimic-rule state))))))))
