(defpackage #:parser
  (:use #:cl #:alexandria #:cond #:atn #:parser-states #:parser-creation #:lazy-list #:lexer #:parser-transformation)
  (:export #:defparser
           #:parser))
(in-package #:parser)

(defvar *grammar*)

(defun head-parse-str (str)
  `(head (lexer ',*grammar* (make-string-input-stream ,str))))

(defun fn-name-for-rule (rule)
  (symbolicate "%%" rule))

(defun mk-term (type results)
  `(values (list (make-instance ',type) ,@(reverse results)) stream))

(defun mk-term* (type result)
  `(values (cons (make-instance ',type) ,result) stream))

(defun error-lex (stream type)
  (if (eq :eof (first (head stream)))
      (error "Unexpected EOF at ~A:~A. Token of type ~A expected."
             (third (head stream))
             (fourth (head stream))
             type)
      (error "Unexpected token \"~A\" of type ~A at ~A:~A. Token of type ~A expected."
             (second (head stream))
             (first (head stream))
             (third (head stream))
             (fourth (head stream))
             type)))

(defun error-str (stream str)
  (if (eq :eof (first (head stream)))
      (error "Unexpected EOF at ~A:~A. \"~A\" expected."
             (third (head stream))
             (fourth (head stream))
             str)
      (error "Unexpected token \"~A\" of type ~A at ~A:~A. \"~A\" expected."
             (second (head stream))
             (first (head stream))
             (third (head stream))
             (fourth (head stream))
             str)))

(defun error-alt (stream alts)
  (if (eq :eof (first (head stream)))
      (error "Unexpected EOF at ~A:~A. Either of ~A expected."
             (third (head stream))
             (fourth (head stream))
             alts)
      (error "Unexpected token \"~A\" of type ~A at ~A:~A. Either of ~A expected."
             (second (head stream))
             (first (head stream))
             (third (head stream))
             (fourth (head stream))
             alts)))

(defun state->cond (state)
  (let ((first (first-for-state state)))
    (labels ((%first->cond (x)
               (if (eq x :eps) 
                   (mappend #'%first->cond (follow-for-state state))
                   (ecase (first x)
                     (:str
                      (let ((str `(once (second ,(head-parse-str (second x))))))
                        `((eq ,str (second (head stream))))))
                     (:lex
                      `((eq ',(second x) (first (head stream)))))))))
      `(or ,@(mappend #'%first->cond first)))))

(defun states-list->action (states results &optional mimic?)
  (if (null (rest states))
      (state->action (first states) results)
      `(cond
         ,@(mapcar (lambda (s)
                     `(,(state->cond s)
                       ,(state->action s results)))
                   (remove-if (lambda (s)
                                (or (@typep s 'end-state)
                                    (@typep s 'eps-state)))
                              states))
         ,(if-let (eps (first (member-if (rcurry #'@typep 'eps-state) states)))
            `(t ,(state->action (first (@state-nexts eps)) results))
            (if-let (end (first (member-if (rcurry #'@typep 'end-state) states)))
              (if mimic?
                  (if (cdr results)
                      (error "Mimic rule must be the only one in chain")
                      `(t (values ,(first results) stream)))
                  `(t ,(state->action end results)))
              `(t (error "No alt!")))))))

(def-state-generic state->action (state results))

(def-state-method state->action ((state rule-state) results)
  (let ((result-sym (gensym)))
    `(multiple-value-bind (,result-sym stream)
         (,(fn-name-for-rule (@state-rule state)) stream)
       ,(states-list->action (@state-nexts state) (cons result-sym results)))))

(def-state-method state->action ((state lex-state) results)
  (let ((result-sym (gensym)))
    `(if (eq ',(@state-lex state) (first (head stream)))
         (let ((,result-sym (second (head stream)))
               (stream (tail stream)))
           ,(states-list->action (@state-nexts state) (cons result-sym results)))
         (parser::error-lex stream ',(@state-lex state)))))

(def-state-method state->action ((state str-state) results)
  (let ((str `(once (second ,(head-parse-str (@state-str state))))))
    `(if (eq ,str (second (head stream)))
         (let ((stream (tail stream)))
           ,(states-list->action (@state-nexts state) results))
         (parser::error-str stream ',(@state-str state)))))

(def-state-method state->action ((state mimic-state) results)
  (if (null (rest (@state-nexts state))) 
      `(,(@state-mimic-rule state) stream)
      (let ((result-sym (gensym)))
        `(multiple-value-bind (,result-sym stream)
             (,(fn-name-for-rule (@state-mimic-rule state)) stream)
           ,(states-list->action (@state-nexts state) (cons result-sym results) t)))))

(def-state-method state->action ((state end-state) results)
  (mk-term (@state-end-type state) results))

;; Rule

(def-rule-generic rule->body (rule))

(def-rule-method rule->body ((rule simple-rule))
  (state->action (first (@rule-state rule)) nil))

(def-rule-method rule->body ((rule or-rule))
  (states-list->action (@rule-state rule) nil))

(defun make-loop-helper-fn-body (fn-name rule body-cond result)
  (let ((result1 (gensym))
        (delim (if (eq :eps (@loop-delim rule))
                   :eps
                   `(once (second ,(head-parse-str (@loop-delim rule)))))))
    `(multiple-value-bind (,result stream)
         (,(fn-name-for-rule (@state-rule (first (@rule-state rule)))) stream)
       (if ,(if (eq :eps delim)
                body-cond
                `(eq ,delim (second (head stream))))
           (multiple-value-bind (,result1 stream)
               (,fn-name (tail stream))
             (values (cons ,result ,result1) stream))
           (values (list ,result) stream)))))

(def-rule-method rule->body ((rule s-loop-rule))
  (let ((helper-fn (gensym))
        (result (gensym))
        (body-cond (state->cond (first (@rule-state rule)))))
    `(if ,body-cond
         (labels ((,helper-fn (stream)
                    ,(make-loop-helper-fn-body helper-fn rule body-cond result)))
           (multiple-value-bind (,result stream)
               (,helper-fn stream)
             ,(mk-term* rule result)))
         ,(mk-term rule nil))))

(def-rule-method rule->body ((rule p-loop-rule))
  (let* ((helper-fn (gensym))
         (result (gensym))
         (body-first (first-for-state (first (@rule-state rule))))
         (body-cond (state->cond (first (@rule-state rule)))))
    `(if ,body-cond
         (labels ((,helper-fn (stream)
                    ,(make-loop-helper-fn-body helper-fn rule body-cond result)))
           (multiple-value-bind (,result stream)
               (,helper-fn stream)
             ,(mk-term* rule result)))
         (parser::error-alt stream ',body-first))))

(defun rules->defs ()
  (let (body)
    (mapc (lambda (r)
            (let ((mimic (getf (@rule-options r) :mimic)))
              (push `(defterm ,(symbolicate "%%" r) (,(or mimic 'term)) nil) body)))
          (@rules))
    (@traverse-atn
     (lambda (s)
       (when (@typep s 'p-end-state)
         (let ((rule (get-rule-for-state s)))
          (push `(defterm ,(@state-end-type s) (,(symbolicate "%%" rule)) ,(@state-end-format s)) body)))))
    (reverse body)))

(defmacro defparser (grammar &body rules)
  (let ((*grammar* grammar))
    (with-atn (transform (grammar->atn rules)) 
      (let ((parser-sym (gensym)))
        `(progn
           ,@(rules->defs)

           (defun ,parser-sym (stream start-rule)
             (labels ,(mapcar (lambda (rule)
                                `(,(fn-name-for-rule rule) (stream)
                                  ,(rule->body rule)))
                              (@rules))
               (case start-rule
                 ,@(mapcar (lambda (r)
                             `(,r (,(fn-name-for-rule r) stream)))
                    (@rules)))))

           (defmethod parser ((grammar (eql ',grammar)) stream &optional (start-rule ',(@extra :start-rule)))
             (,parser-sym stream start-rule)))))))



(defgeneric parser (grammar stream &optional start-rule))

(defclass term () ())

(defmethod print-object ((term term) stream)
  (princ (type-of term) stream))

(defgeneric term-format (term))

(defmacro defterm (name superterms format)
  `(progn
     (defclass ,name ,superterms ())
     (defmethod term-format ((term ,name))
       ',format)))

(let ((%%vals (make-hash-table)))
  (defmacro once (&body body)
    (let* ((val-sym (gensym))
           (fn (lambda (val-fn)
                 (multiple-value-bind (val has?)
                     (gethash val-sym %%vals)
                   (if has?
                       val
                       (setf (gethash val-sym %%vals) (funcall val-fn)))))))
      `(funcall ,fn (lambda () ,@body)))))
