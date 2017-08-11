(defpackage #:cl-yatlp/transformations/opt-rule
  (:use #:cl #:alexandria #:cl-yatlp/atn #:cl-yatlp/parser-states)
  (:export #:add-opt-rules))

(in-package #:cl-yatlp/transformations/opt-rule)

(defun opt-rule? (rule)
  (let ((name (symbol-name rule)))
    (char= #\? (aref name (1- (length name))))))

(defun star-rule? (rule)
  (let ((name (symbol-name rule)))
    (char= #\* (aref name (1- (length name))))))

(defun plus-rule? (rule)
  (let ((name (symbol-name rule)))
    (char= #\+ (aref name (1- (length name))))))

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
    (@add-state mimic-end 'p-end-state :type name
                                       :options nil
                                       :format '(:_))
    (@add-state eps 'eps-state :nexts (list eps-end))
    (@add-state eps-end 'p-end-state :type name
                                     :options nil
                                     :format nil)
    (@add-rule name 'or-rule :state (list eps mimic)
                             :options nil)))

(defun add-star-rule (name)
  (let* ((rule-name (symbol-name name))
         (real-rule (subseq rule-name 0 (1- (length rule-name))))
         (real-rule-sym (intern real-rule (symbol-package name)))
         (body-sym (gensym))
         (end-sym (gensym)))
    (@add-rule name 's-loop-rule :state (list body-sym)
                                      :delim :eps
                                      :options nil)
    (@add-state body-sym 'rule-state :nexts (list end-sym)
                                     :rule real-rule-sym)
    (@add-state end-sym 'p-end-state :type name :format '(:* :eps :_))))

(defun add-plus-rule (name)
  (let* ((rule-name (symbol-name name))
         (real-rule (subseq rule-name 0 (1- (length rule-name))))
         (real-rule-sym (intern real-rule (symbol-package name)))
         (body-sym (gensym))
         (end-sym (gensym)))
    (@add-rule name 'p-loop-rule :state (list body-sym)
                                      :delim :eps
                                      :options nil)
    (@add-state body-sym 'rule-state :nexts (list end-sym)
                                     :rule real-rule-sym)
    (@add-state end-sym 'p-end-state :type name :format '(:+ :eps :_))))

(defun add-opt-rules ()
  (@traverse-atn
   (lambda (state)
     (cond
       ((@typep state 'rule-state)
        (cond
          ((opt-rule? (@state-rule state))
           (add-opt-rule (@state-rule state)))
          ((star-rule? (@state-rule state))
           (add-star-rule (@state-rule state)))
          ((plus-rule? (@state-rule state))
           (add-plus-rule (@state-rule state)))))
       ((@typep state 'mimic-state)
        (cond
          ((opt-rule? (@state-mimic-rule state))
           (add-opt-rule (@state-mimic-rule state)))
          ((star-rule? (@state-mimic-rule state))
           (add-star-rule (@state-mimic-rule state)))
          ((plus-rule? (@state-mimic-rule state))
           (add-plus-rule (@state-mimic-rule state)))))))))
