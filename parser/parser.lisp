(defpackage #:parser
  (:use #:cl #:alexandria #:cond #:atn #:parser-states #:lazy-list #:lexer)
  (:export ;;#:defparser
           ;;#:parser
   ))

(in-package #:parser)

(defgeneric state-for (rule-type format state-name next-state))

(defmethod state-for (rule-type format state-name next-state)
  (error "Wrong sub-rule format ~A" format))

(defmethod state-for ((rule-type (eql :call)) format state-name next-state)
  (@add-state state-name 'rule-state :nexts (list next-state)
                                     :rule format))

(defmethod state-for ((rule-type (eql :str)) format state-name next-state)
  (@add-state state-name 'str-state :nexts (list next-state)
                                    :str format))

(defmethod state-for ((rule-type (eql :eps)) format state-name next-state)
  (@add-state state-name 'eps-state :nexts (list next-state)))

(defmethod state-for ((rule-type (eql :lex)) format state-name next-state)
  (@add-state state-name 'lex-state :nexts (list next-state)
                                    :lex (second format)))

(defmethod state-for ((rule-type (eql :^)) format state-name next-state)
  (@add-state state-name 'mimic-state :nexts (list next-state)
                                      :rule (second format)))

(defmethod state-for ((rule-type (eql :seq)) format state-name next-state)
  (if (null (rest format))
      (desr-rule->state (first format) state-name next-state)
      (let ((seq-end (gensym)))
        (desr-rule->state (first format) state-name seq-end)
        (state-for :seq (rest format) seq-end next-state))))

(defun desr-rule->state (format state-name next-state)
  (cond
    ((null format)
     (error "Wrong format"))
    ((stringp format)
     (state-for :str format state-name next-state))
    ((eq :eps format)
     (state-for :eps format state-name next-state))
    ((symbolp format)
     (state-for :call format state-name next-state))
    ((listp format)
     (if (member (first format) '(:^ :lex))
         (state-for (first format) format state-name next-state)
         (state-for :seq format state-name next-state)))
    (t (error "Wrong rule format ~A" format))))

(defgeneric rule-for (rule-type format rule-name options))

(defmethod rule-for ((rule-type (eql ':or)) format rule-name options)
  (@add-rule rule-name 'or-rule
             :state (mapcar (lambda (f)
                              (let ((alter (gensym))
                                    (end-sym (gensym)))
                                (@add-state end-sym 'end-state :type rule-name)
                                (desr-rule->state f alter end-sym)
                                alter))
                            (rest format))
             :options options))

(defmethod rule-for ((rule-type (eql ':*)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 's-loop-rule :state (list body-sym)
                                        :delim delim)
      (@add-state end-sym 'end-state :type rule-name)
      (desr-rule->state body body-sym end-sym))))

(defmethod rule-for ((rule-type (eql ':+)) format rule-name options)
  (destructuring-bind (delim &rest body)
      (rest format)
    (let ((body-sym (gensym))
          (end-sym (gensym)))
      (@add-rule rule-name 'p-loop-rule :state (list body-sym)
                                        :delim delim)
      (@add-state end-sym 'end-state :type rule-name)
      (desr-rule->state body body-sym end-sym))))

(defun rule->state (rule)
  (destructuring-bind (name format &rest options)
      rule
    (cond
      ((null format)
       (error "Wrong format"))
      ((and (listp format)
            (member (first format) '(:or :* :+)))
       (rule-for (first format) format name options))
      (t
       (let ((state-name (gensym))
             (end-name (gensym)))
         (@add-rule name 'simple-rule :state (list state-name)
                                      :options options)
         (@add-state end-name 'end-state :type name)
         (desr-rule->state format state-name end-name))))))

(defun grammar->atn (grammar)
  (let ((atn (make-atn))) 
    (with-atn atn 
      (dolist (rule grammar)
        (rule->state rule))
      atn)))

;;;;;;;;

(defun uniquify-names (states)
  (let ((suffix 1))
    (labels ((%uniquify (state)
               (if (@typep state 'end-state)
                   (progn
                     (unless (= suffix 1)
                         (setf (@state-end-type state) (symbolicate (@state-end-type state)
                                                                    (format nil "-~A" suffix))))
                     (incf suffix))
                   (mapc #'%uniquify (@state-nexts state)))))
      (mapc #'%uniquify states))))

(def-rule-generic transform-rule (rule))

(def-rule-method transform-rule ((rule rule))
  (declare (ignore rule)))

(def-rule-method transform-rule ((rule or-rule))
  (let ((rules (make-hash-table))
        (lexs (make-hash-table))
        (strs (make-hash-table))
        (has-eps (member-if (rcurry #'@typep 'eps-state) (@rule-state rule))))
    (loop for s in (@rule-state rule)
          if (@typep s 'rule-state)
            do (push s (gethash (@state-rule s) rules))
          if (@typep s 'mimic-state)
            do (push s (gethash (@state-mimic-rule s) rules))
          if (@typep s 'lex-state)
            do (push s (gethash (@state-lex s) lexs))
          if (@typep s 'str-state)
            do (push s (gethash (@state-str s) strs)))
    (setf (atn::rule-state (@get-rule rule)) nil)
    (dolist (states (hash-table-values rules))
      (if (= 1 (length states))
          (push (first states) (atn::rule-state (@get-rule rule)))
          (let ((state (or (first (member-if (rcurry #'@typep 'mimic-state) states))
                           (first states))))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (atn::rule-state (@get-rule rule))))))
    (dolist (states (hash-table-values lexs))
      (if (= 1 (length states))
          (push (first states) (atn::rule-state (@get-rule rule)))
          (let ((state (first states)))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (atn::rule-state (@get-rule rule))))))
    (dolist (states (hash-table-values strs))
      (if (= 1 (length states))
          (push (first states) (atn::rule-state (@get-rule rule)))
          (let ((state (first states)))
            (setf (@state-nexts state)
                  (mappend #'@state-nexts states))
            (push state (atn::rule-state (@get-rule rule))))))
    (if has-eps
        (push (first has-eps) (atn::rule-state (@get-rule rule))))
    (uniquify-names (@rule-state rule))))

(defun transform (atn)
  (with-atn atn
    (mapc #'transform-rule (@rules))
    atn))

;;;;;;;;

(def-state-generic first-for-state (state))

(def-state-method first-for-state ((state rule-state))
  (let ((rule-first (first-for-rule (@state-rule state))))
    (if (member :eps rule-first)
        (if (member-if (rcurry #'@typep 'end-state) (@state-nexts state))
            (if (= 1 (length (@state-nexts state)))
                rule-first
                (remove-duplicates
                 (append (mappend #'first-for-state (remove-if (rcurry #'@typep 'end-state)
                                                               (@state-nexts state)))
                         rule-first)))
            (remove-duplicates
             (append (mappend #'first-for-state (remove-if (rcurry #'@typep 'end-state)
                                                           (@state-nexts state)))
                     (remove :eps rule-first))))
        rule-first)))

(def-state-method first-for-state ((state lex-state))
  (list (list :lex (@state-lex state))))

(def-state-method first-for-state ((state str-state))
  (list (list :str (@state-str state))))

(def-state-method first-for-state ((state eps-state))
  (list :eps))

(def-state-method first-for-state ((state mimic-state))
  (first-for-rule (@state-mimic-rule state)))

(defun first-for-rule (rule)
  (mappend #'first-for-state (@rule-state rule)))

;;;;;;;;

(defun follow-for-rule (rule)
  )

;;;;;;;;

(defun first->cond (first)
  `(or ,@(mapcar (lambda (x)
                   (ecase (first x)
                     (:str
                      (let ((str (second (head (lexer 'signal (make-string-input-stream (second x)))))))
                        `(eq ',str (second (head stream)))))
                     (:lex
                      `(eq ',(second x) (first (head stream))))))
                 first)))

(defun states-list->action (states results &optional mimic?)
  (if (= 1 (length states))
      (state->action (first states) results)
      `(cond
         ,@(mapcar (lambda (s)
                     `(,(first->cond (first-for-state s))
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
        (body-cond (first->cond (first-for-state (first (@rule-state rule))))))
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
         (body-cond (first->cond body-first)))
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
