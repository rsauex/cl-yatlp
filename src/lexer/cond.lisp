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

;;; Cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cond - is a structure which describes the set of chars
;; It can be either:
;;  - <char> - a single character (e.g. #\a, #\])
;;  - (:r <char1> <char2>) - range from <char1> to <char2> (e.g. (:r #\a #\z), (:r #\0 #\9))
;;  - (:~ <form>...) - everything excluding inner froms (e.g. (:~ #\a (:r #\e #\h)))
;;  - (<form>...) - or of forms (e.g. (#\a #\b #\c))
;;  - nil - nothing
;;  - t - every character

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
  "Returns the type of cond. Error if cond is malformed.
Return value: :char :list :~ :r :t :nil."
  (cond
    ((characterp el)
     :char)
    ((eq t el)
     :t)
    ((eq nil el)
     :nil)
    ((listp el)
     (if (keywordp (first el))
         (first el)
         :list))
    (t (error "Malformed cond: ~A" el))))

;; Helper functions

(defun next-char (char)
  (code-char (1+ (char-code char))))

(defun prev-char (char)
  (code-char (1- (char-code char))))

(defun char-in-range? (char range)
  (when (and (char>= char (second range))
             (char<= char (third range)))
    char))

;;; Operations on conds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defop cond-intersection (x y)
  ((:char :char)
   (when (eq x y) x))
  ((:char :r)
   (char-in-range? x y))
  ((:list :char)
   (reduce #'cond-union (mapcar (curry #'cond-intersection y) x)))
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
   (unless (cond-intersection x (rest y))
     x))
  ((:r :~)
   (cond-difference x (rest y)))
  ((:list :~)
   (cond-difference x (rest y)))
  ((:~ :~)
   (list :~ (cond-union (rest x) (rest y))))
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
       ((and (char<= (second y) (second x))
             (char>= (third y) (third x)))
        nil)
       ((char< (second y) (second x))
        (if (char= next (third x))
            next
            (list :r next (third x))))
       ((char> (third y) (third x))
        (if (char= prev (second x))
            prev
            (list :r (second x) prev))))))
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
   (let ((prev (prev-char (second y)))
         (next (next-char (third y))))
    (cond
      ((char< prev x next)
       y)
      ((char= x next)
       (list :r (second y) next))
      ((char= x prev)
       (list :r prev (third y)))
      (t
       (list x y)))))
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
   (let ((prev-x (prev-char (second x)))
         (next-x (next-char (third x))))
     (cond
       ((char= next-x (second y))
        (list :r (second x) (third y)))
       ((char= prev-x (third y))
        (list :r (second y) (third x)))
       ((or (char< next-x (second y))
            (char> (second x) (third y)))
        (list x y))
       (t
        (let ((a (if (char> (second y) (second x)) (second x) (second y)))
              (b (if (char< (third y) (third x)) (third x) (third y))))
          (list :r a b))))))
  ((:char :~)
   (if (cond-equal x (rest y))
       t
       (list :~ (cond-difference (rest y) x))))
  ((:r :~)
   (if (cond-equal x (rest y))
       t
       (list :~ (cond-difference (rest y) x))))
  ((:list :~)
   (if (cond-equal x (rest y))
       t
       (list :~ (cond-difference (rest y) x))))
  ((:~ :~)
   (if-let (it (cond-intersection (rest x) (rest y)))
     (if (eq :list (cond-type it))
         (cons :~ it)
         (list :~ it))
     t))
  ((:t   t)     t)
  ((:nil t)     y))

(defun cond-equal (x y)
  (not (cond-optimize (cond-union (cond-difference x y)
                                  (cond-difference y x)))))

;;; Cond optimization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %flatten-cond (x)
  (ecase (cond-type x)
    (:r
     (list x))
    (:~
     (list (list* :~ (%flatten-cond (rest x)))))
    (:list
     (mappend #'%flatten-cond x))
    ((:char :t :nil)
     (list x))))

(defun flatten-cond (x)
  (case (cond-type x)
    (:~
     (list* :~ (%flatten-cond (rest x))))
    (:list
     (let ((new-x (%flatten-cond x)))
       (if (rest new-x)
           new-x
           (first new-x))))
    ((:char :r :t :nil)
     x)))

(defgeneric %cond-optimize (type x))

(defmethod %cond-optimize (type x)
  x)

(defmethod %cond-optimize ((type (eql :list)) x)
  (labels ((%separate (values &optional chars rs not)
             (if (null values)
                 (values (remove-duplicates chars :test #'char=)
                         rs
                         not)
                 (ecase (cond-type (first values))
                   (:~
                    (%separate (rest values) chars rs (if not
                                                          (cond-intersection not (rest (first values)))
                                                          (rest (first values)))))
                   (:char
                    (%separate (rest values) (cons (first values) chars) rs not))
                   (:r
                    (%separate (rest values) chars (cons (first values) rs) not))
                   (:nil
                    (%separate (rest values) chars rs not))))))
    (let ((new-x (mapcar #'cond-optimize x)))
      (or (first (member t new-x))
          (multiple-value-bind (chars rs not)
              (%separate new-x)
            (if not
                (cond-optimize (list :~ (cond-difference not (list chars rs))))
                (append chars rs)))))))

(defmethod %cond-optimize ((type (eql :r)) x)
  (if (char= (second x) (third x))
      (second x)
      x))

(defmethod %cond-optimize ((type (eql :~)) x)
  (let ((value (cond-optimize (rest x))))
    (ecase (cond-type value)
      (:t
       nil)
      (:nil
       t)
      (:~
       (rest value))
      (:list
       (list* :~ value))
      ((:char :r)
       (list :~ value)))))

(defun cond-optimize (condition)
  "Canonicalizates cond. Is called internaly after each operation."
  (let ((condition (flatten-cond condition)))
    (flatten-cond
     (%cond-optimize (cond-type condition) condition))))

;;; Miscellaneous ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cond->test (cond var)
  (case (cond-type cond)
    (:char
     `(char= ,cond ,var))
    (:r
     `(char<= ,(second cond) ,var ,(third cond)))
    (:~
     `(not ,(cond->test (rest cond) var)))
    (:list
     `(or ,@(mapcar (rcurry #'cond->test var) cond)))
    (:t
     t)
    (t
     (error "Cannot transform cond to test: ~A" cond))))

(defun possible-values (conds)
  "For a set of conds returns an equivalent disjoint-set."
  (labels ((%clear (conds)
             (remove-duplicates
              (remove nil conds)
              :test #'cond-equal))
           (%all-intersections (conds)
             (%clear
              (loop for (c . rest) on conds
                    append (mapcar (curry #'cond-intersection c) rest)))))
    (let ((inters (%all-intersections conds)))
      (if (null inters)
          conds
          (possible-values
           (%clear
            (reduce #'cons conds
                    :initial-value inters
                    :from-end t
                    :key (rcurry #'cond-difference inters))))))))
