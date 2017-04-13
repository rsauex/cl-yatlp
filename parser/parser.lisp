(defpackage #:parser
  (:use #:cl #:alexandria #:cond #:atn #:parser-states #:parser-creation #:lazy-list #:lexer #:parser-transformation)
  (:export ;;#:defparser
           ;;#:parser
   ))
(in-package #:parser)

(defun state->cond (state)
  (let ((first (first-for-state state)))
    (labels ((%first->cond (x)
               (if (eq x :eps) 
                   (progn (print (@state-type state)) (mappend #'%first->cond (print (follow-for-state (print state)))))
                   (ecase (first x)
                     (:str
                      (let ((str (second (head (lexer 'signal (make-string-input-stream (second x)))))))
                        `((eq ',str (second (head stream))))))
                     (:lex
                      `((eq ',(second x) (first (head stream)))))))))
      `(or ,@(mappend #'%first->cond
                      first)))))

(defun states-list->action (states results &optional mimic?)
  (if (= 1 (length states))
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
         (,(@state-rule state) stream)
       ,(states-list->action (@state-nexts state) (cons result-sym results)))))

(def-state-method state->action ((state lex-state) results)
  (let ((result-sym (gensym)))
    `(if (eq ',(@state-lex state) (first (head stream)))
         (let ((,result-sym (second (head stream)))
               (stream (tail stream)))
           ,(states-list->action (@state-nexts state) (cons result-sym results)))
         (if (eq :eof (first (head stream)))
             (error "Unexpected EOF at ~A:~A. Token of type ~A expected."
                    (third (head stream))
                    (fourth (head stream))
                    ',(@state-lex state))
             (error "Unexpected token \"~A\" of type ~A at ~A:~A. Token of type ~A expected."
                    (second (head stream))
                    (first (head stream))
                    (third (head stream))
                    (fourth (head stream))
                    ',(@state-lex state))))))

(def-state-method state->action ((state str-state) results)
  (let ((str (second (head (lexer 'signal (make-string-input-stream (@state-str state)))))))
    `(if (eq ',str (second (head stream)))
         (let ((stream (tail stream)))
           ,(states-list->action (@state-nexts state) results))
         (if (eq :eof (first (head stream)))
             (error "Unexpected EOF at ~A:~A. \"~A\" expected."
                    (third (head stream))
                    (fourth (head stream))
                    ',(@state-str state))
             (error "Unexpected token \"~A\" of type ~A at ~A:~A. \"~A\" expected."
                    (second (head stream))
                    (first (head stream))
                    (third (head stream))
                    (fourth (head stream))
                    ',(@state-str state))))))

(def-state-method state->action ((state mimic-state) results)
  (if (= 1 (length (@state-nexts state))) 
      `(,(@state-mimic-rule state) stream)
      (let ((result-sym (gensym)))
        `(multiple-value-bind (,result-sym stream)
             (,(@state-mimic-rule state) stream)
           ,(states-list->action (@state-nexts state) (cons result-sym results) t)))))

(def-state-method state->action ((state end-state) results)
  `(values (list ',(@state-end-type state) ,@(reverse results)) stream))

(def-rule-generic rule->body (rule))

(def-rule-method rule->body ((rule simple-rule))
  (state->action (first (@rule-state rule)) nil))

(def-rule-method rule->body ((rule or-rule))
  (states-list->action (@rule-state rule) nil))

(def-rule-method rule->body ((rule s-loop-rule))
  (let ((helper-fn (gensym))
        (result (gensym))
        (result1 (gensym))
        (delim (if (eq :eps (@loop-delim rule))
                   :eps
                   (second (head (lexer 'signal (make-string-input-stream (@loop-delim rule)))))))
        (body-cond (state->cond (first (@rule-state rule)))))
    `(if ,body-cond
         (labels ((,helper-fn (stream)
                    (multiple-value-bind (,result stream)
                        (,(@state-rule (first (@rule-state rule))) stream)
                      ,(cond
                         ((eq :eps delim)
                          `(if ,body-cond
                               (multiple-value-bind (,result1 stream)
                                   (,helper-fn stream)
                                 (values (cons ,result ,result1) stream))
                               (values (list ,result) stream)))
                         (t
                          `(if (eq ',delim (second (head stream)))
                               (multiple-value-bind (,result1 stream)
                                   (,helper-fn (tail stream))
                                 (values (cons ,result ,result1) stream))
                               (values (list ,result) stream)))))))
           (multiple-value-bind (,result stream)
               (,helper-fn stream)
             (values (cons ',rule ,result) stream)))
         (values (list ',rule) stream))))

(def-rule-method rule->body ((rule p-loop-rule))
  (let* ((helper-fn (gensym))
         (result (gensym))
         (result1 (gensym))
         (delim (if (eq :eps (@loop-delim rule))
                    :eps
                    (second (head (lexer 'signal (make-string-input-stream (@loop-delim rule)))))))
         (body-first (first-for-state (first (@rule-state rule))))
         (body-cond (state->cond (first (@rule-state rule)))))
    `(if ,body-cond
         (labels ((,helper-fn (stream)
                    (multiple-value-bind (,result stream)
                        (,(@state-rule (first (@rule-state rule))) stream)
                      ,(cond
                         ((eq :eps delim)
                          `(if ,body-cond
                               (multiple-value-bind (,result1 stream)
                                   (,helper-fn stream)
                                 (values (cons ,result ,result1) stream))
                               (values (list ,result) stream)))
                         (t
                          `(if (eq ',delim (second (head stream)))
                               (multiple-value-bind (,result1 stream)
                                   (,helper-fn (tail stream))
                                 (values (cons ,result ,result1) stream))
                               (values (list ,result) stream)))))))
           (multiple-value-bind (,result stream)
               (,helper-fn stream)
             (values (cons ',rule ,result) stream)))
         (if (eq :eof (first (head stream)))
             (error "Unexpected EOF at ~A:~A. Either of ~A expected."
                    (third (head stream))
                    (fourth (head stream))
                    ',body-first)
             (error "Unexpected token \"~A\" of type ~A at ~A:~A. Either of ~A expected."
                    (second (head stream))
                    (first (head stream))
                    (third (head stream))
                    (fourth (head stream))
                    ',body-first)))))

(defun atn->labels (atn)
  (with-atn atn
    `(labels
         ,@(mapcar (lambda (rule)
                     `(,rule (stream)
                        ,(rule->body rule)))
                   (@rules)))))

(defmacro defparser (grammar &body rules)
  (declare (ignore grammar))
  (with-atn (transform (grammar->atn rules)) 
    (progn ;;let ((parser-sym (gensym)))
      `(progn
         (defun parser (stream)
           (labels ,(mapcar (lambda (rule)
                              `(,rule (stream)
                                      ,(rule->body rule)))
                            (@rules))
             (,(first (@rules)) stream)))))))
