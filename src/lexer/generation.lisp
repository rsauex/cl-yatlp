(uiop:define-package #:cl-yatlp/src/lexer/generation
  (:use #:cl #:alexandria #:cl-yatlp/src/lexer/cond #:cl-yatlp/src/atn #:cl-yatlp/src/lexer/states 
        #:cl-yatlp/src/lexer/creation #:cl-yatlp/src/lexer/transformation)
  (:export #:deflexer
           #:lexer))

(in-package #:cl-yatlp/src/lexer/generation)

;;; Lexer runtime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-state-generic state->action (state grammar)
  (:documentation "Returns body for state"))

(def-state-method state->action ((state simple-state) grammar)
  `(cond
     ((null *cur*)
      (error "Unexpected EOF at ~A:~A" *line* *pos*))
     ,@(mapcar (lambda (s)
                 `(,(cond->test (next-cond s) '*cur*)
                   (next ,(next-state s))))
               (@state-nexts state))
     (t
      (error "Unexpected symbol ~S at ~A:~A" *cur* *line* *pos*))))

(def-state-method state->action ((state end-state) grammar)
  (let ((return-form
          `(return
             (list ',(if-let (skip? (member :skip (@state-end-options state)))
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
                      `(out-no-add))
                   start-line
                   start-pos))))
    `(cond
       ((null *cur*)
        ,return-form)
       ,@(mapcar (lambda (s)
                   `(,(cond->test (next-cond s) '*cur*)
                     (next ,(next-state s))))
                 (@state-nexts state))
       (t ,return-form))))

(defun states->body (grammar-name)
  (mappend (lambda (s)
             `(,s ,(state->action s grammar-name)))
           (@states)))

(defun grammar->tagbody (grammar grammar-name)
  (with-atn (nfa->dfa (grammar->nfa grammar))
    `(block nil
       (tagbody
          (go ,(@extra :start))
          ,@(states->body grammar-name)))))

;;; Lexer runtime ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro out-no-add ()
  `(concatenate 'string (reverse *cur-res*)))

(defun register-lexer (grammar fn)
  (setf (gethash grammar cl-yatlp/src/common::*lexer-grammars*) fn))

(defmacro deflexer (grammar &body rules)
  (let ((lexer-sym (gensym)))
    `(progn
       (uiop:define-package ,grammar)
       (defun ,lexer-sym ()
         (let ((*cur-res* nil)
               (start-line *line*)
               (start-pos *pos*))
           (when (null *cur*)
             (return-from ,lexer-sym (list :eof nil *line* (1+ *pos*))))
           ,(grammar->tagbody rules grammar)))
       (cl-yatlp/src/lexer/generation::register-lexer ,(intern (symbol-name grammar) :keyword) #',lexer-sym))))

(defun lexer (stream grammar)
  (let* ((lexer-fn (gethash grammar cl-yatlp/src/common::*lexer-grammars*))
         (*stream* stream)
         (*cur* nil)
         (*line* 1)
         (*pos* 0)
         (*returnp* nil)
         (*newlinep* nil))
    (load-cur)
    (loop for token = (funcall lexer-fn)
          unless (eq :skip (first token))
            collect token into tokens
          when (eq :eof (first token))
            return tokens)))
