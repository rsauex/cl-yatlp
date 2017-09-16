(defpackage #:cl-yatlp/cond
  (:use #:cl #:alexandria)
  (:export #:cond-union
           #:cond-difference
           #:cond-intersection
           #:cond-equal
           #:cond-optimize
           #:cond->test
           #:possible-values))

(in-package #:cl-yatlp/cond)

(defmacro defop (name params &body body)
  (let ((type-params (loop for param in params
                           collect (gensym)))
        (generic-fn-name (symbolicate "%" name)))
    (labels ((%make-method (form-params body)
               (let ((type-params
                       (mapcar (lambda (sym type)
                                 (if (eq t type)
                                     sym
                                     `(,sym (eql ,type))))
                               type-params form-params)))
                 `(defmethod ,generic-fn-name (,@type-params
                                               ,@params)
                    ,@body))))
      `(progn
         (defgeneric ,generic-fn-name (,@type-params ,@params))
         ,@(mapcar (lambda (entry)
                     (destructuring-bind (i-types &rest i-body)
                         entry
                       `(progn
                          ,(%make-method i-types i-body)
                          ,(unless (or (eq (first i-types) (second i-types))
                                       (member (reverse i-types) body :key #'first :test #'equal))
                             (%make-method (reverse i-types) `((,generic-fn-name ,@i-types ,@(reverse params))))))))
                   body)
         (defun ,name ,params
           (cond-optimize
            (,generic-fn-name ,@(mapcar (curry #'list 'cond-type) params)
                              ,@params)))))))

(defun cond-type (el)
  (cond
    ((characterp el)
     :char)
    ((eq :eps el)
     :eps)
    ((eq t el)
     :t)
    ((eq nil el)
     :nil)
    ((listp el)
     (if (keywordp (first el))
         (first el)
         :list))
    (t (error "Malformed cond: ~A" el))))

(defun next-char (char)
  (code-char (1+ (char-code char))))

(defun prev-char (char)
  (code-char (1- (char-code char))))

(defun char-in-range? (char range)
  (when (and (char>= char (second range))
             (char<= char (third range)))
    char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defop cond-intersection (x y) 
  ((:char :char)
   (when (eq x y) x))
  ((:char :r)
   (char-in-range? x y))
  ((:char :list)
   (when (some (curry #'cond-intersection x) y)
     x))
  ((:list :r)
   (reduce #'cond-union (mapcar (curry #'cond-intersection y) x)))
  ((:list :list)
   (reduce #'cond-union (mapcar (curry #'cond-intersection y) x)))
  ((:r :r)
   (let ((a (if (char< (second y) (second x)) (second x) (second y)))
         (b (if (char> (third y) (third x)) (third x) (third y))))
     (if (char< a b)
         (list :r a b)
         (when (char= a b) a))))
  ((:char :~)
   (unless (cond-intersection x (rest y)) x))
  ((:r :~)
   (cond-difference x (rest y)))
  ((:list :~)
   (cond-difference x (rest y)))
  ((:~ :~)
   (cond-union x y))
  ((:eps :char)
   nil)
  ((:eps :list)
   (reduce #'cond-union (mapcar (curry #'cond-intersection :eps) y)))
  ((:eps :r)
   nil)
  ((:eps :~)
   nil)
  ((:eps :eps)
   :eps)
  ((:nil t)
   nil)
  ((:t t)
   y))

(defop cond-difference (x y)
  ((:char :char)
   (unless (eq x y) x))
  ((:char :list)
   (unless (member-if (curry #'cond-intersection x) y)
     x))
  ((:char :r)
   (unless (char-in-range? x y)
     x))
  ((:r :char)
   (let ((prev (prev-char y))
         (next (next-char y)))
     (cond
       ((not (char-in-range? y x))
        x)
       ((and (char> y (second x))
             (char< y (third x)))
        (list (if (char= prev (second x))
                  prev
                  (list :r (second x) prev))
              (if (char= next (third x))
                  next
                  (list :r next (third x)))))
       ((char= y (second x))
        (if (char= next (third x))
            next
            (list :r next (third x))))
       ((char= y (third x))
        (if (char= prev (second x))
            prev
            (list :r (second x) prev))))))
  ((:r :r)
   (let ((prev (prev-char (second y)))
         (next (next-char (third y))))
     (cond
       ((or (char< (third y) (second x))
            (char> (second y) (third x)))
        x)
       ((and (char> (second y) (second x))
             (char< (third y) (third x)))
        (list (if (char= prev (second x))
                  prev
                  (list :r (second x) prev))
              (if (char= next (third x))
                  next
                  (list :r next (third x)))))
       ((char= (third y) (second x))
        (if (char= next (third x))
            next
            (list :r next (third x))))
       ((char= (second y) (third x))
        (if (char= prev (second x))
            prev
            (list :r (second x) prev)))
       ((char= (second y) (second x))
        (when (char< (third y) (third x))
          (list :r (next-char (third y)) (third x))))
       ((char= (third y) (third x))
        (when (char> (second y) (second x))
          (list :r (second x) (prev-char (second y))))))))
  ((:r :list)
   (reduce #'cond-intersection (mapcar (curry #'cond-difference x) y)))
  ((:list :char)
   (reduce #'cond-union (mapcar (rcurry #'cond-difference y) x)))
  ((:list :list)
   (reduce #'cond-union (mapcar (rcurry #'cond-difference y) x)))
  ((:list :r)
   (reduce #'cond-union (mapcar (rcurry #'cond-difference y) x)))
  ((:char :~)
   (cond-intersection x (rest y)))
  ((:r :~)
   (cond-intersection x (rest y)))
  ((:list :~)
   (cond-intersection x (rest y)))
  ((:~ :~)
   (cond-intersection x (rest y)))
  ((:~ :char)
   (list :~ (cond-union (rest x) y)))
  ((:~ :r)
   (list :~ (cond-union (rest x) y)))
  ((:~ :list)
   (list :~ (cond-union (rest x) y)))
  ((:~ :~)
   (cond-intersection x (rest y)))
  ((:eps t)
   :eps)
  ((t :eps)
   x)
  ((t :t)
   nil)
  ((:t t)
   (list :~ y))
  ((:nil t)
   nil)
  ((t :nil)
   x))

(defop cond-union (x y)
  ((:char :char)
   (if (char/= x y)
       (list x y)
       x))
  ((:char :r)
   (if (char-in-range? x y)
       y
       (list x y)))
  ((:char :list)
   (if (cond-intersection x y)
       y
       (cons x y)))
  ((:list :r)
   (if-let (it (cond-intersection x y))
     (if (cond-equal it y)
         x
         (cons (cond-difference y it) x))
     (cons y x)))
  ((:list :list)
   (if (cond-intersection x y)
       (if-let (z (cond-difference y x))
         (if (eq :list (cond-type z))
             (append x z)
             (cons z x))
         x)
       (append x y)))
  ((:r :r)
   (if (or (char< (third x) (second y))
           (char> (second x) (third y)))
       (list x y)
       (let ((a (if (char> (second y) (second x)) (second x) (second y)))
             (b (if (char< (third y) (third x)) (third x) (third y))))
         (list :r a b))))
  ((:char :~)
   (if (cond-equal x (rest y))
       t
       (if (cond-intersection x y)
         y
         (list x y))))
  ((:r :~)
   (if (cond-equal x (rest y))
       t
       (if-let (res (cond-intersection x y))
         (if (cond-equal res x)
             y
             (list y (cond-difference x y)))
         (list x y))))
  ((:list :~)
   (if (cond-equal x (rest y))
       t
       (if-let (res (cond-intersection x y))
         (if (cond-equal res x)
             y
             (list x (cond-difference y x)))
         (cons y x))))
  ((:~ :~)
   (if (cond-intersection (rest x) (rest y))
       (list x (cond-difference y x))
       t))
  ((:eps t)     (list x y))
  ((:t   t)     t)
  ((:nil t)     y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flatten-if-not (predicate tree)
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (and (consp subtree)
                          (not (funcall predicate subtree)))
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

(defgeneric %cond-optimize (type x))

(defmethod %cond-optimize ((type (eql :list)) x)
  (remove-duplicates
   (mapcar #'cond-optimize x)))

(defmethod %cond-optimize ((type (eql :r)) x)
  (if (char= (second x) (third x))
      (second x)
      x))

(defmethod %cond-optimize ((type (eql :~)) x)
  x)

(defun flatten-cond (condition)
  (if (and (listp condition)
           (not (eq :r (first condition))))
      (let ((res (mapcar (lambda (c)
                           (if (and (listp c)
                                    (eq :~ (first c)))
                               (cons :~ (mapcar #'flatten-cond (rest c)))
                               c))
                         (flatten-if-not (lambda (x) (keywordp (first x)))
                                         condition))))
        (if (= 1 (length res))
            (first res)
            res))
      condition))

(defun cond-optimize (condition)
  (let ((condition (flatten-cond condition)))
    ;; (print condition)
    ;; (break)
    (cond
      ((null condition)
       condition)
      ((listp condition)
       (if (keywordp (first condition))
           (%cond-optimize (first condition) condition)
           (%cond-optimize :list condition)))
      (t condition))))

(defun cond-equal (x y)
  (not (cond-optimize (cond-union (cond-difference x y)
                                  (cond-difference y x)))))

(defun cond->test (cond var)
  (cond
    ((characterp cond)
     `(char= ,cond ,var))
    ((listp cond)
     (case (first cond)
       (:r
        `(and (char>= ,var ,(second cond))
              (char<= ,var ,(third cond))))
       (:~
        `(not ,(cond->test (rest cond) var)))
       (t
        `(or ,@(mapcar (rcurry #'cond->test var) cond)))))
    ((eq t cond)
     t)
    (t
     (error "Malformed cond: ~A" cond))))

(defun possible-values (conds)
  "For a set of conds returns an equivalent disjoint-set."
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
