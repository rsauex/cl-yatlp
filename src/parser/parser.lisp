(defpackage #:cl-yatlp/parser
  (:use #:cl #:alexandria #:cl-yatlp/cond #:atn #:cl-yatlp/parser-states #:cl-yatlp/parser-creation #:lazy-list #:cl-yatlp/lexer #:cl-yatlp/parser-transformation)
  (:export #:defparser
           #:parser))

(in-package #:cl-yatlp/parser)

(defvar *grammar*)

(defun head-parse-str (str)
  `(head (lexer (make-string-input-stream ,str) ',*grammar*)))

(defun fn-name-for-rule (rule)
  (symbolicate "%%" rule))

(defun mk-term (type results)
  `(values (list (make-instance ',type) ,@(reverse results)) stream))

(defun mk-term* (type result)
  `(values (cons (make-instance ',type) ,result) stream))

(defun parser-error (stream type &rest values)
  (let ((last-part
          (case type
            (:lex "Token of type ~A expected.")
            (:str "\"~A\" expected.")
            (:alt "Either of ~A expected.")
            (:cons "Rule ~A does not consume all the input."))))
    (if (eq :eof (first (head stream)))
        (apply #'error
               (concatenate 'string "Unexpected EOF at ~A:~A. " last-part)
               (third (head stream))
               (fourth (head stream))
               values)
        (apply #'error
               (concatenate 'string "Unexpected token \"~A\" of type ~A at ~A:~A. " last-part)
               (second (head stream))
               (first (head stream))
               (third (head stream))
               (fourth (head stream))
               values))))

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
              `(t (cl-yatlp/parser::parser-error stream :alt ',(mappend #'first-for-state
                                                               states))))))))

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
         (cl-yatlp/parser::parser-error stream :lex ',(@state-lex state)))))

(def-state-method state->action ((state str-state) results)
  (let ((str `(once (second ,(head-parse-str (@state-str state))))))
    `(if (eq ,str (second (head stream)))
         (let ((stream (tail stream)))
           ,(states-list->action (@state-nexts state) results))
         (cl-yatlp/parser::parser-error stream :str ',(@state-str state)))))

(def-state-method state->action ((state mimic-state) results)
  (if (null (rest (@state-nexts state))) 
      `(,(fn-name-for-rule (@state-mimic-rule state)) stream)
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
               (,fn-name ,(if (eq :eps delim)
                              `stream
                              `(tail stream)))
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
         (cl-yatlp/parser::parser-error stream :alt ',body-first))))

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
  (let ((*grammar* (make-keyword grammar)))
    (with-atn (transform (grammar->atn rules)) 
      (let ((parser-sym (gensym)))
        `(progn
           ,@(rules->defs)

           (defun ,parser-sym (stream start-rule)
             (labels ,(mapcar (lambda (rule)
                                `(,(fn-name-for-rule rule) (stream)
                                  ,(rule->body rule)))
                              (@rules))
               (multiple-value-bind (res stream-rest)
                   (case start-rule
                     ,@(mapcar (lambda (r)
                                 `(,r (,(fn-name-for-rule r) stream)))
                        (@rules)))
                 (unless (eq :eof (first (head stream-rest)))
                   (cl-yatlp/parser::parser-error stream-rest :cons start-rule))
                 res)))

           (defmethod parser ((grammar (eql ,(make-keyword grammar))) stream &optional (start-rule ',(@extra :start-rule)))
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


(defun print-term (term &optional (stream *standard-output*))
  (let ((*standard-output* stream)
        (*print-pretty* t)
        (*print-right-margin* 88)
        (*print-miser-width* nil)
        (no-space? t))
    (labels ((%print-loop (format delim components)
               (when components
                 (unless (eq :eps delim)
                   (princ delim))
                 (%print-with-format format (list (first components)))
                 (%print-loop format delim (rest components))))
             (%print-with-format (format components)
               (pprint-newline :fill)
               (when format
                 (if (stringp (first format))
                     (progn
                       (if no-space?
                           (setf no-space? nil)
                           (princ " "))
                       (princ (first format))
                       (%print-with-format (rest format) components))
                     (case (first format)
                       (:{
                        (multiple-value-bind (format comps)
                            (pprint-logical-block (nil nil)
                              (%print-with-format (rest format) components))
                          (%print-with-format format comps)))
                       (:}
                        (values format components))
                       (:v
                        (pprint-newline :mandatory)
                        (setf no-space? t)
                        (%print-with-format (rest format) components))
                       (:>
                        (pprint-indent :block 3)
                        (%print-with-format (rest format) components))
                       (:<
                        (pprint-indent :block 0)
                        (%print-with-format (rest format) components))
                       (:.
                        (setf no-space? t)
                        (%print-with-format (rest format) components))
                       (:!
                        (pprint-indent :current 1)
                        (%print-with-format (rest format) components))
                       (:lex
                        (if no-space?
                            (setf no-space? nil)
                            (princ " "))
                        (princ (first components))
                        (%print-with-format (rest format) (rest components)))
                       (:_
                        (%print (first components))
                        (%print-with-format (rest format) (rest components)))
                       ((:+ :*)
                        (when components
                          (%print-with-format (rest (rest format)) (list (first components)))
                          (%print-loop (rest (rest format)) (second format) (rest components))))))))
             (%print (term)
               (let ((format (term-format (first term))))
                 (when format
                   (%print-with-format format (rest term))))))
      (pprint-logical-block (*standard-output* nil)
        (%print term)))))

(defun parse-str (grammar str rule)
  (parser grammar (lexer (make-string-input-stream str) grammar) rule))
