(defpackage #:lexer-transformation
  (:use #:cl #:alexandria #:cond #:atn #:lexer-states #:lexer-creation)
  (:export #:transf-atn))

(in-package #:lexer-transformation)

(def-state-generic add-to-new-normalized-rule (state &optional in-loop?))
(def-state-generic get-for-nexts (state))

(defun possible-values (conds)
  (labels ((%all-intersections (c rest acc)
             (if (null rest)
                 acc
                 (%all-intersections
                  (first rest)
                  (rest rest)
                  (reduce (lambda (acc rest-c)
                            (if-let (res (cond-intersection c rest-c))
                              (cons res acc)
                              acc))
                          rest :initial-value acc)))))
    (let ((inters (remove-duplicates (%all-intersections (first conds) (rest conds) nil))))
      (if (null inters)
          conds
          (possible-values
           (remove-duplicates
            (reduce (lambda (acc c)
                      (if-let (res (cond-difference c inters))
                        (cons res acc)
                        acc))
                    conds :initial-value inters)))))))

(defun get-nexts-for-call (state nexts)
  (ecase (@state-type state)
    (simple-state
     (if (eq t (@state-cond state))
         (mappend (lambda (s) (get-nexts-for-call s nexts))
                  (@state-nexts state))
         (let ((new-sym (gensym)))
           (@add-state new-sym 'simple-state :cond (@state-cond state)
                                             :nexts (mapcar (lambda (s)
                                                              (let ((next-call (gensym)))
                                                                (@add-state next-call 'call-state :call-to s 
                                                                                                  :nexts nexts)
                                                                next-call))
                                                            (@state-nexts state)))
           (list new-sym))))
    (end-state
     (mappend #'get-nexts nexts))))

(defun copy-call (state nexts &optional (state-name (gensym)))
  (if (@typep state 'end-state)
      nexts
      (progn
        (@add-state state-name (@state-type state)
                    :cond (@state-cond state)
                    :nexts (mappend (lambda (s)
                                      (copy-call s nexts (gensym)))
                                    (@state-nexts state)))
        (list state-name))))

(def-state-method get-for-nexts ((state simple-state))
  (list state))

(def-state-method get-for-nexts ((state eps-state))
  (get-nexts state))

(def-state-method get-for-nexts ((state end-state))
  (list state))

(def-state-method get-for-nexts ((state loop-state))
  (add-to-new-normalized-rule state)
  (@state-nexts state))

(def-state-method get-for-nexts ((state ng-loop-state))
  (add-to-new-normalized-rule state t)
  (if-let (end (get-best-end (@state-nexts state)))
    (list end)
    (@state-nexts state)))

;; TODO: call not fragment rule
(def-state-method get-for-nexts ((state call-state))
  (add-to-new-normalized-rule (@state-call-to state))
  (let ((new-nexts
          (copy-call (@state-call-to state) (@state-nexts state))))
    (@add-state state 'eps-state :nexts new-nexts)
    new-nexts))

(defun get-nexts (state)
  (let ((nexts (@state-nexts state)))
    (loop for next in nexts
          appending (get-for-nexts next))))

(defun new-nexts-for-conds (old-nexts conds &optional in-loop?)
  (mapcar (lambda (cond)
            (let ((nexts (mappend (lambda (n)
                                    (when (cond-intersection cond (@state-cond n))
                                      (@state-nexts n)))
                                  old-nexts))
                  (id (if-let (id (first (member-if (lambda (s) (cond-equal cond (@state-cond s)))
                                                    old-nexts)))
                        id
                        (let ((id (gensym)))
                          (@add-state id 'simple-state :cond cond)
                          id))))
              (let ((end (get-best-end nexts))) 
                (if (and end in-loop?)
                    (setf (@state-nexts id) (list end))
                    (setf (@state-nexts id) nexts)))
              id))
          conds))

(defun get-best-end (nexts)
  (let* ((ends (remove-if-not (lambda (s) (@typep s 'end-state)) nexts))
         (ends-types (mapcar #'@state-end-type ends))
         (name (first (member-if (lambda (name) (member name ends-types)) (@extra :order)))))
    (first (member-if (lambda (s) (eq name (@state-end-type s))) ends))))

(def-state-method add-to-new-normalized-rule ((state state) &optional in-loop?)
  (unless (visit state)
    (let* ((state-nexts-r (get-nexts state))
           (end-state (get-best-end state-nexts-r))
           (state-nexts (remove-if (lambda (s) (@typep s 'end-state)) state-nexts-r))
           (old-nexts-conds (mapcar #'@state-cond state-nexts))
           (new-nexts-conds (possible-values old-nexts-conds))
           (new-nexts (if (tree-equal old-nexts-conds new-nexts-conds)
                          state-nexts
                          (new-nexts-for-conds state-nexts new-nexts-conds in-loop?)))
           (new-nexts-with-end (if end-state
                                   (cons end-state (if in-loop?
                                                       (remove-if (rcurry #'@typep 'loop-state) new-nexts)
                                                       new-nexts))
                                   new-nexts)))
      (setf (@state-nexts state) new-nexts-with-end)
      ;;(@atn->dot) (break)
      (mapc (rcurry #'add-to-new-normalized-rule in-loop?) new-nexts-with-end))))

(def-state-method add-to-new-normalized-rule ((state end-state) &optional in-loop?)
  (declare (ignore in-loop?))
  (visit state))

(defun remove-unreachable-states (&optional (start :start))
  (with-visiting
    (labels ((%visit (state)
               (unless (visit state)
                 (mapc #'%visit (@state-nexts state)))))
      (%visit start)
      (mapc (lambda (s)
              (unless (visit s)
                (@rem-state s)))
            (@states)))))

(defun transf-atn (atn)
  (with-atn atn
    (with-visiting
      (add-to-new-normalized-rule :start))
    (remove-unreachable-states)
    atn))
