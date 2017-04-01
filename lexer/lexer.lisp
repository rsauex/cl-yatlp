(defpackage #:lexer
  (:use #:cl #:alexandria #:cond #:atn #:lexer-states 
        #:lexer-creation #:lexer-transformation #:lazy-list)
  (:export #:deflexer
           #:lexer))

(in-package #:lexer)

(defun atn->tagbody (atn fun-name)
  (with-atn atn
    (mappend (lambda (s)
               (cond
                 ((@typep s 'simple-state)
                  `(,s
                    (cond
                      ,(if-let (end (first (member-if (lambda (s) (@typep s 'end-state)) (@state-nexts s))))
                         `((null *cur*)
                           (go ,end))
                         `((null *cur*)
                           ,(if (eq :start s)
                                `(return-from ,fun-name (values :eof nil *line* *pos*))
                                `(error "Unexpected EOF at ~A:~A" *line* *pos*))))
                      ,@(mapcar (lambda (s)
                                  `(,(cond->test (@state-cond s) '*cur*)
                                    (next ,s)))
                                (remove-if (lambda (s) (@typep s 'end-state)) (@state-nexts s)))
                      (t
                       ,(if-let (end (first (member-if (lambda (s) (@typep s 'end-state)) (@state-nexts s))))
                          `(go ,end)
                          `(progn
                             (warn "Unexpected symbol ~S at ~A:~A" *cur* *line* *pos*)
                             (read-char *stream* nil)
                             (load-cur)
                             ,(if (eq :start s)
                                  `(return-from ,fun-name (values :skip "" *line* *pos*))
                                  `(go ,s))))))))
                 ((@typep s 'end-state)
                  `(,s
                    (return-from ,fun-name (values ',(if-let (skip? (member :skip (@state-end-options s)))
                                                       (if (second skip?)
                                                           :skip
                                                           (@state-end-type s))
                                                       (@state-end-type s))
                                                   ,(if-let (transform (member :transform (@state-end-options s)))
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
                                                      `(intern (out-no-add)))
                                                   *line*
                                                   *pos*))))))
             (hash-table-keys (atn::atn-table atn)))))






(defvar *stream*)
(defvar *res-stream*)
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

(defgeneric lexer (grammar str))

(defmacro deflexer (grammar &body rules)
  (let ((lexer-sym (gensym)))
    `(progn
       (defun ,lexer-sym ()
         (tagbody
            (go :start)
            ,@(atn->tagbody (transf-atn (grammar->atn rules)) lexer-sym)))
       (defmethod lexer ((grammar (eql ',grammar)) stream)
         (let* ((*stream* stream)
                (*res-stream* nil)
                (*cur-res* nil)
                (*cur* nil)
                (*line* 1)
                (*pos* 0)
                (*returnp* nil)
                (*newlinep* nil))
           (labels ((%lexer (*stream* *res-stream* *cur-res* *cur* *line* *pos* *returnp* *newlinep*)
                      (let ((start-line *line*)
                            (start-pos *pos*))
                        (multiple-value-bind (type text)
                            (,lexer-sym)
                          (let ((stream *stream*)
                                (cur *cur*)
                                (line *line*)
                                (pos *pos*)
                                (returnp *returnp*)
                                (newlinep *newlinep*))
                            (if (eq :skip type)
                                (%lexer stream nil nil cur line pos returnp newlinep)
                                (unless (eq type :eof)
                                  (lcons (list type text start-line start-pos)
                                         (%lexer stream nil nil cur line pos returnp newlinep)))))))))
             (load-cur)
             (%lexer *stream* nil nil *cur* *line* *pos* *returnp* *newlinep*)))))))
