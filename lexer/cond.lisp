(defpackage #:cond
  (:use #:cl #:alexandria)
  (:export #:cond-union
           #:cond-difference
           #:cond-intersection
           #:cond-equal
           #:cond-optimize
           #:cond->test))

(in-package #:cond)

(defmacro defop (name params &body body)
  (let ((type-params (loop for param in params
                           collect (gensym))))
    (labels ((%make-method (form-params body)
               `(defmethod ,name (,@(mapcar (lambda (sym type) `(,sym (eql ,type)))
                                            type-params form-params)
                                  ,@params)
                  ,@body)))
      `(progn
         ,@(mapcar (lambda (entry)
                     (destructuring-bind (i-types &rest i-body)
                         entry
                       `(progn
                          ,(%make-method i-types i-body)
                          ,(unless (or (eq (first i-types) (second i-types))
                                       (member (reverse i-types) body :key #'first :test #'equal))
                             (%make-method (reverse i-types) `((,name ,@i-types ,@(reverse params))))))))
                   body)))))

(defun cond-type (el)
  (cond
    ((characterp el)
     :char)
    ((listp el)
     (if (keywordp (first el))
         (first el)
         :list))
    ((eq t el)
     t)
    (t (error "Malformed cond: ~A" el))))

(defun next-char (char)
  (code-char (1+ (char-code char))))

(defun prev-char (char)
  (code-char (1- (char-code char))))

(defun char-in-range? (char range)
  (when (and (char>= char (second range))
             (char<= char (third range)))
    char))

(defgeneric %cond-union (type1 type2 x y))
(defgeneric %cond-difference (type1 type2 x y))
(defgeneric %cond-intersection (type1 type2 x y))

(defop %cond-intersection (x y) 
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
  ((:range :~)
   (cond-difference x (rest y)))
  ((:list :~)
   (cond-difference x (rest y)))
  ((:~ :~)
   (cond-union x y)))

(defun cond-intersection (x y)
  (cond-optimize
   (when (and x y)
     (cond
       ((eq t x) y)
       ((eq t y) x)
       (t (%cond-intersection (cond-type x) (cond-type y) x y))))))

(defop %cond-difference (x y)
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
   (cond-intersection x (rest y))))

(defun cond-difference (x y)
  (cond-optimize
   (cond
     ((eq t y)
      x)
     ((eq t x)
      (cond
        ((eq t y)
         nil)
        ((eq :~ (cond-type y))
         (rest y))
        (t
         (list :~ y))))
     ((not (or x y))
      nil)
     ((null x)
      nil)
     ((null y)
      x)
     (t
      (%cond-difference (cond-type x) (cond-type y) x y)))))

(defop %cond-union (x y)
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
       t)))

(defun cond-union (x y)
  (cond
    ((or (eq t x)
         (eq t y))
     t)
    ((null x)
     y)
    ((null y)
     x)
    (t
     (%cond-union (cond-type x) (cond-type y) x y))))

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
  (reduce #'cond-union x :initial-value nil))

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
    ;;(print condition)
    ;;(break)
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
    (t
     (error "Malformed cond: ~A" cond))))
