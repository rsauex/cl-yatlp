(defpackage #:cl-yatlp/lexer-transformation
  (:use #:cl #:alexandria #:cl-yatlp/cond #:cl-yatlp/atn #:cl-yatlp/lexer-states #:cl-yatlp/lexer-creation)
  (:export #:nfa->dfa))

(in-package #:cl-yatlp/lexer-transformation)

;; (def-state-generic convert-state (state &optional in-loop?))
;; (def-state-generic get-for-nexts (state))

;; (defun possible-values (conds)
;;   (labels ((%all-intersections (conds)
;;              (loop for (c . rest) on conds
;;                    append (remove nil (reduce #'cons rest
;;                                               :initial-value nil
;;                                               :from-end t
;;                                               :key (lambda (rest-c) (cond-intersection c rest-c)))))))
;;     (let ((inters (remove-duplicates (%all-intersections conds))))
;;       (if (null inters)
;;           conds
;;           (possible-values
;;            (remove-duplicates
;;             (remove nil
;;                     (reduce #'cons conds
;;                             :initial-value inters
;;                             :from-end t
;;                             :key (lambda (c) (cond-difference c inters))))))))))

;; (defun copy-call (state nexts &optional (state-id (gensym)))
;;   (if (@typep state 'end-state)
;;       nexts
;;       (progn
;;         (@add-state state-id (@state-type state)
;;                     :cond (@state-cond state)
;;                     :nexts (mappend (lambda (s)
;;                                       (copy-call s nexts (gensym)))
;;                                     (@state-nexts state)))
;;         (list state-id))))

;; (def-state-method get-for-nexts ((state simple-state))
;;   (list state))

;; (def-state-method get-for-nexts ((state eps-state))
;;   (get-nexts state))

;; (def-state-method get-for-nexts ((state end-state))
;;   (list state))

;; (def-state-method get-for-nexts ((state loop-state))
;;   (convert-state state)
;;   (@state-nexts state))

;; (def-state-method get-for-nexts ((state ng-loop-state))
;;   (convert-state state t)
;;   (if-let (end (get-best-end (@state-nexts state)))
;;     (list end)
;;     (@state-nexts state)))

;; ;; TODO: call not fragment rule
;; (def-state-method get-for-nexts ((state call-state))
;;   (@add-state state 'eps-state :nexts (copy-call (@state-call-to state) (@state-nexts state)))
;;   (get-for-nexts state))

;; (defun get-nexts (state)
;;   (mappend #'get-for-nexts (@state-nexts state)))

;; (defun merge-ids (ids)
;;   (let (to-remove)
;;     (values (loop for (cur-id . rest) on ids
;;                   unless (member cur-id to-remove)
;;                     collect (let ((ids--same-nexts (remove-if (lambda (id) (set-difference (@state-nexts id) (@state-nexts cur-id))) rest)))
;;                               (when ids--same-nexts
;;                                 (mapc (lambda (id) (push id to-remove)) ids--same-nexts)
;;                                 (setf (@state-cond cur-id) (reduce #'cond-union ids--same-nexts :key #'@state-cond :initial-value (@state-cond cur-id))))
;;                               cur-id))
;;             to-remove)))

;; (defun new-nexts-for-conds (old-nexts conds old-conds &optional in-loop?)
;;   (if (every #'cond-equal conds old-conds)
;;       old-nexts
;;       (let ((new-nexts
;;               (mapcar (lambda (cond)
;;                         (let ((nexts (mappend (lambda (n)
;;                                                 (when (cond-intersection cond (@state-cond n))
;;                                                   (@state-nexts n)))
;;                                               old-nexts))
;;                               (id (if-let (id (first (member-if (lambda (s) (cond-equal cond (@state-cond s)))
;;                                                                 old-nexts)))
;;                                     id
;;                                     (let ((id (gensym)))
;;                                       (@add-state id 'simple-state :cond cond)
;;                                       id))))
;;                           (let ((end (get-best-end nexts))) 
;;                             (if (and end in-loop?)
;;                                 (setf (@state-nexts id) (list end))
;;                                 (setf (@state-nexts id) nexts)))
;;                           id))
;;                       conds)))
;;         new-nexts)))

;; (defun get-best-end (nexts)
;;   (let* ((ends (remove-if-not (rcurry #'@typep 'end-state) nexts))
;;          (name (first (member-if (rcurry #'member ends :key #'@state-end-type) (@extra :order)))))
;;     (first (member-if (lambda (s) (eq name (@state-end-type s))) ends))))

;; (defun get-nexts-and-end (state)
;;   (let ((state-nexts (get-nexts state)))
;;     (values (remove-if (rcurry #'@typep 'end-state) state-nexts)
;;             (get-best-end state-nexts))))

;; (defun new-nexts (state-nexts in-loop? end-state)
;;   (let* ((old-nexts-conds (mapcar #'@state-cond state-nexts))
;;          (new-nexts-conds (possible-values old-nexts-conds))
;;          (new-nexts (new-nexts-for-conds state-nexts new-nexts-conds old-nexts-conds in-loop?)))
;;     (if end-state
;;         (cons end-state (if in-loop?
;;                             (remove-if (rcurry #'@typep 'loop-state) new-nexts)
;;                             new-nexts))
;;         new-nexts)))

;; (def-state-method convert-state ((state state) &optional in-loop?)
;;   (unless (visit state)
;;     (multiple-value-bind (state-nexts end-state)
;;         (get-nexts-and-end state)
;;       (let ((new-nexts (new-nexts state-nexts in-loop? end-state)))
;;         (setf (@state-nexts state) new-nexts)
;;         (mapc (rcurry #'convert-state in-loop?) new-nexts)))))

;; (def-state-method convert-state ((state end-state) &optional in-loop?)
;;   (declare (ignore in-loop?))
;;   (visit state))

;; (defun remove-unreachable-states (&optional (start :start))
;;   (with-visiting
;;     (labels ((%visit (state)
;;                (unless (visit state)
;;                  (mapc #'%visit (@state-nexts state)))))
;;       (%visit start)
;;       (mapc (lambda (s)
;;               (unless (visit s)
;;                 (@rem-state s)))
;;             (@states)))))

;; (defun merge-states (&optional (start :start))
;;   (with-visiting
;;     (labels ((%visit (state)
;;                (unless (visit state)
;;                  (setf (@state-nexts state) (merge-ids (@state-nexts state)))
;;                  (mapc #'%visit (@state-nexts state)))))
;;       (%visit start)))
;;   (remove-unreachable-states))

;;; New version ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-call (state nexts &optional (state-id (gensym)))
  (if (@typep state 'end-state)
      (@add-state state-id 'simple-state :cond t :nexts nexts)
      (@add-state state-id (@state-type state)
                  :cond (@state-cond state)
                  :nexts (mapcar (lambda (s)
                                   (print (list (copy-call (first s) nexts (gensym))
                                                (second s))))
                                 (print(@state-nexts state)))))
  state-id)

(defun insert-calls ()
  (@traverse-atn
   (lambda (id)
     (when (@typep id 'call-state)
       (@add-state id 'simple-state
                   :cond t
                   :nexts (list (list (copy-call (@state-call-to id) (@state-nexts id)) :eps)))))))

(defun nfa->dfa (nfa)
  "Transforms the given nfa into dfa, which is the returning value.
The original nfa is modified."
  (with-atn nfa
    (insert-calls)
    ;; (with-visiting
    ;;   (convert-state :start))
    ;; (remove-unreachable-states)
    ;;(merge-states)
    )
  nfa)



;;; Algorythm:
;; 1. insert called bodys
;; 2. nfa -> dfa
;; 3. minimise dfa
