(defpackage #:cl-yatlp/lexer-transformation
  (:use #:cl #:alexandria #:cl-yatlp/cond #:cl-yatlp/atn #:cl-yatlp/lexer-states #:cl-yatlp/lexer-creation)
  (:export #:nfa->dfa))

(in-package #:cl-yatlp/lexer-transformation)

(defun possible-values (conds)
  (labels ((%all-intersections (conds)
             (loop for (c . rest) on conds
                   append (remove nil (reduce #'cons rest
                                              :initial-value nil
                                              :from-end t
                                              :key (lambda (rest-c) (cond-intersection c rest-c)))))))
    (let ((inters (remove-duplicates (%all-intersections conds))))
      (if (null inters)
          conds
          (possible-values
           (remove-duplicates
            (remove nil
                    (reduce #'cons conds
                            :initial-value inters
                            :from-end t
                            :key (lambda (c) (cond-difference c inters))))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; New version ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call insertaion

(defun copy-call (state nexts &optional (state-id (gensym)))
  (if (@typep state 'end-state)
      (@add-state state-id 'simple-state :cond t :nexts nexts)
      (@add-state state-id (@state-type state)
                  :cond (@state-cond state)
                  :nexts (mapcar (lambda (s)
                                   (list (copy-call (first s) nexts (gensym))
                                         (second s)))
                                 (@state-nexts state))))
  state-id)

(defun insert-calls ()
  (@traverse-atn
   (lambda (id)
     (when (@typep id 'call-state)
       (@add-state id 'simple-state
                   :cond t
                   :nexts (list (list (copy-call (@state-call-to id) (@state-nexts id)) :eps)))))))

;; NFA -> DFA

(def-state-generic %eps-closure (state))

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
  (with-visiting
    (sort (%eps-closure state)
          #'string< :key #'symbol-name)))

(defun eps-closure-for-set (set)
  (sort (remove-duplicates
         (mappend #'eps-closure set))
        #'string< :key #'symbol-name))

;;

(defun conds-for-set (set)
  (possible-values
   (remove-duplicates
    (remove :eps
            (mappend (lambda (state)
                       (mapcar (lambda (next)
                                 (second next))
                               (@state-nexts state)))
                     set))
    :test #'cond-equal)))

(defun nexts-for (set cond)
  (eps-closure-for-set
   (mappend (lambda (state)
              (mappend (lambda (next)
                         (and (cond-equal cond (cond-intersection cond (second next)))
                              (list (first next))))
                       (@state-nexts state)))
            set)))

;;

(defvar *sets*)

(defun register-set (set &optional (name (gensym)))
  (setf (gethash set *sets*) name))

(defun try-register-set (set &optional (name (gensym)))
  (if-let (old-name (gethash set *sets*))
    old-name
    (values (register-set set name) t)))

(defun n->d/set (set)
  (multiple-value-bind (set-name new?)
      (try-register-set set)
    (unless new?
      (return-from n->d/set set-name))

    (let* ((sigma (conds-for-set set)))
      (@add-state set-name 'simple-state
                  :cond t
                  :nexts (mapcar (lambda (cond)
                                   (let ((nexts (nexts-for set cond)))
                                     (list (n->d/set nexts) cond)))
                                 sigma))
      set-name)))

(defun n->d/state (state)
  (let* ((eps-cl (eps-closure state))
         (set-name (register-set eps-cl))
         (sigma (conds-for-set eps-cl)))
    (@add-state set-name 'simple-state
                :cond t
                :nexts (mapcar (lambda (cond)
                                 (let ((nexts (nexts-for eps-cl cond)))
                                   (list (n->d/set nexts) cond)))
                               sigma))
    set-name))

(defun n->d ()
  (let ((*sets* (make-hash-table :test #'equalp)))
    (n->d/state :start)))

;; Main entry point

(defun nfa->dfa (nfa)
  "Transforms the given nfa into dfa, which is the returning value.
The original nfa is modified."
  (with-atn nfa
    (insert-calls)
    (n->d)
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
