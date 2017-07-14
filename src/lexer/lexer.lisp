(defpackage #:cl-yatlp/lexer
  (:use #:cl #:alexandria #:lazy-list #:cl-yatlp/cond #:atn #:cl-yatlp/lexer-states 
        #:cl-yatlp/lexer-creation #:cl-yatlp/lexer-transformation)
  (:export #:deflexer
           #:lexer))

(in-package #:cl-yatlp/lexer)

(def-state-generic state->action (state fn-name grammar))

(def-state-method state->action ((state simple-state) fn-name grammar)
  (let ((next-end (first (member-if (rcurry #'@typep 'end-state) (@state-nexts state))))
        (nexts-without-end (remove-if (rcurry #'@typep 'end-state) (@state-nexts state))))
    `(cond
       ((null *cur*)
        ,(if next-end
             `(go ,next-end)
             `(error "Unexpected EOF at ~A:~A" *line* *pos*)))
       ,@(mapcar (lambda (s)
                   `(,(cond->test (@state-cond s) '*cur*)
                     (next ,s)))
                 nexts-without-end)
       (t
        ,(if next-end
             `(go ,next-end)
             `(progn
                (warn "Unexpected symbol ~S at ~A:~A" *cur* *line* *pos*)
                (read-char *stream* nil)
                (load-cur)
                ,(if (eq :start state)
                     `(return-from ,fn-name (values :skip "" *line* *pos*))
                     `(go ,state))))))))

(def-state-method state->action ((state end-state) fn-name grammar)
  `(return-from ,fn-name
     (values ',(if-let (skip? (member :skip (@state-end-options state)))
                 (if (second skip?)
                     :skip
                     (@state-end-type state))
                 (@state-end-type state))
             ,(if-let (transform (member :transform (@state-end-options state)))
                (cond
                  ((symbolp (second transform))
                   `',(second transform))
                  ((listp transform)
                   (cond
                     ((null transform)
                      `(out-no-add))
                     ((eq 'function (first (second transform)))
                      `(funcall ,(second transform) (out-no-add)))
                     ((eq 'quote (first (second transform)))
                      (second transform)))))
                `(intern (out-no-add) ',grammar))
             *line*
             *pos*)))

(defun grammar->tagbody (rules fn-name grammar)
  (with-atn (nfa->dfa (grammar->nfa rules))
    `(tagbody
        (go :start)
        ,@(mappend (lambda (s)
                     `(,s ,(state->action s fn-name grammar)))
                   (@states)))))



(defvar *stream*)
(defvar *cur*)
(defvar *cur-res*)
(defvar *line*)
(defvar *pos*)
(defvar *returnp*)
(defvar *newlinep*)


(defun load-cur ()
  (setf *cur* (peek-char nil *stream* nil))
  (when *cur*
    (incf *pos*)
    (cond
      ((char= *cur* #\Return)
       (when (or *returnp*
                 *newlinep*)
         (incf *line*)
         (setf *pos* 1))
       (setf *returnp* t)
       (setf *newlinep* nil))
      ((char= *cur* #\Newline)
       (when *returnp*
         (setf *returnp* nil))
       (when *newlinep*
         (incf *line*)
         (setf *pos* 1))
       (setf *newlinep* t))
      ((or *newlinep* *returnp*)
       (incf *line*)
       (setf *pos* 1)
       (setf *newlinep* nil)
       (setf *returnp* nil)))))

(defun add ()
  (push *cur* *cur-res*)
  (read-char *stream* nil)
  (load-cur))

(defmacro next (state)
  `(progn (add) (go ,state)))

(defun out ()
  (read-char *stream* nil)
  (concatenate 'string (reverse (cons *cur* *cur-res*))))

(defun out-no-add ()
  (concatenate 'string (reverse *cur-res*)))

(defun register-lexer (grammar fn)
  (setf (gethash grammar cl-yatlp/common::*lexer-grammars*) fn))

(defmacro deflexer (grammar &body rules)
  (let ((lexer-sym (gensym)))
    `(progn
       (defpackage ,grammar)
       (defun ,lexer-sym (stream)
         (let ((*stream* stream)
               (*cur-res* nil))
           (when (null *cur*)
             (return-from ,lexer-sym (values :eof nil *line* *pos*)))
           ,(grammar->tagbody rules lexer-sym grammar)))
       (cl-yatlp/lexer::register-lexer ,(intern (symbol-name grammar) :keyword) #',lexer-sym))))

(defun lexer (stream grammar)
  (let* ((lexer-fn (gethash grammar cl-yatlp/common::*lexer-grammars*))
         (*stream* stream)
         (*cur* nil)
         (*line* 1)
         (*pos* 0)
         (*returnp* nil)
         (*newlinep* nil))
    (labels ((%lexer (*cur* *line* *pos* *returnp* *newlinep*)
               (let ((start-line *line*)
                     (start-pos *pos*))
                 (multiple-value-bind (type text)
                     (funcall lexer-fn stream)
                   (let ((cur *cur*)
                         (line *line*)
                         (pos *pos*)
                         (returnp *returnp*)
                         (newlinep *newlinep*))
                     (if (eq :skip type)
                         (%lexer cur line pos returnp newlinep)
                         (if (eq type :eof)
                             (list (list type text start-line start-pos))
                             (lcons (list type text start-line start-pos)
                                    (%lexer cur line pos returnp newlinep)))))))))
      (load-cur)
      (%lexer *cur* *line* *pos* *returnp* *newlinep*))))
