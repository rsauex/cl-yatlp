(defpackage #:lexer/cond-tests
  (:use #:cl #:cl-yatlp/cond #:lisp-unit2)
  (:import-from #:alexandria
                #:curry))

(in-package #:lexer/cond-tests)

(defmacro deftest (name &body body)
  `(define-test ,name
       (:tags '(:all :cond))
     ,@(mapcar (curry 'list* 'assert-equalp) body)))


(deftest intersection.1
  (#\a (cond-intersection #\a #\a))
  (nil (cond-intersection #\a #\b)))

(deftest intersection.2
  (#\a (cond-intersection #\a '(:r #\a #\c)))
  (#\b (cond-intersection #\b '(:r #\a #\c)))
  (#\c (cond-intersection #\c '(:r #\a #\c)))
  (nil (cond-intersection #\d '(:r #\a #\c))))

(deftest intersection.3
  (#\a (cond-intersection #\a '(#\a #\b #\c)))
  (#\b (cond-intersection #\b '(#\a #\b #\c)))
  (#\c (cond-intersection #\c '(#\a #\b #\c)))
  (nil (cond-intersection #\d '(#\a #\b #\c))))

(deftest intersection.4
  ('(#\c #\b #\a) (cond-intersection '(:r #\a #\c) '(#\a #\b #\c)))
  ('(#\b #\c) (cond-intersection '(:r #\a #\c) '(#\b #\c)))
  ('(#\b #\c) (cond-intersection '(:r #\a #\c) '(#\b #\c)))
  (nil (cond-intersection '(:r #\a #\c) '(#\d #\e))))

(deftest intersection.5
  ('(#\b #\d) (cond-intersection '(#\b #\c #\d) '(#\a #\b #\d)))
  ('#\b (cond-intersection '(#\b #\c) '(#\a #\b #\d)))
  (nil (cond-intersection '(#\b #\c) '(#\a #\d))))

(deftest intersection.6
  (#\c (cond-intersection '(:r #\a #\c) '(:r #\c #\e)))
  ('(:r #\c #\d) (cond-intersection '(:r #\a #\d) '(:r #\c #\e)))
  ('(:r #\d #\f) (cond-intersection '(:r #\d #\f) '(:r #\a #\h)))
  ('(:r #\d #\e) (cond-intersection '(:r #\d #\f) '(:r #\a #\e)))
  (#\d (cond-intersection '(:r #\d #\f) '(:r #\a #\d)))
  (nil (cond-intersection '(:r #\e #\f) '(:r #\a #\d))))

(deftest intersection.7
  (#\c (cond-intersection #\c '(:~ #\a #\b #\d)))
  (nil (cond-intersection #\b '(:~ #\a #\b #\d))))

(deftest intersection.8
  ('(:r #\b #\d) (cond-intersection '(:r #\b #\d) '(:~ #\a)))
  ('((:r #\e #\g) (:r #\a #\c)) (cond-intersection '(:r #\a #\g) '(:~ #\d))))

(deftest intersection.9
  ('(#\c #\a) (cond-intersection '(#\a #\b #\c) '(:~ #\b))))

(deftest intersection.10
  ('(:~ #\a #\b) (cond-intersection '(:~ #\a) '(:~ #\b))))

(deftest intersection.11
  (nil (cond-intersection nil '(#\a #\b #\c)))
  ('(#\c #\b #\a) (cond-intersection t '(#\a #\b #\c))))


(deftest difference.1
  (nil (cond-difference #\a #\a))
  (#\a (cond-difference #\a #\b)))

(deftest difference.2
  (nil (cond-difference #\a '(#\a #\b #\c)))
  (#\a (cond-difference #\a '(#\b #\c))))

(deftest difference.3
  (nil (cond-difference #\b '(:r #\a #\c)))
  (#\a (cond-difference #\a '(:r #\b #\e))))

(deftest difference.4
  ('(:r #\c #\f) (cond-difference '(:r #\b #\f) #\b))
  ('((:r #\e #\f) (:r #\b #\c)) (cond-difference '(:r #\b #\f) #\d))
  ('(:r #\b #\e) (cond-difference '(:r #\b #\f) #\f))
  ('(:r #\b #\f) (cond-difference '(:r #\b #\f) #\a)))

(deftest difference.5
  ('(:r #\c #\h) (cond-difference '(:r #\c #\h) '(:r #\a #\b)))
  ('(:r #\d #\h) (cond-difference '(:r #\c #\h) '(:r #\a #\c)))
  ('(:r #\e #\h) (cond-difference '(:r #\c #\h) '(:r #\a #\d)))
  ('(#\c (:r #\g #\h)) (cond-difference '(:r #\c #\h) '(:r #\d #\f)))
  ('((:r #\g #\h) (:r #\c #\d)) (cond-difference '(:r #\c #\h) '(:r #\e #\f)))
  ('(#\h (:r #\c #\d)) (cond-difference '(:r #\c #\h) '(:r #\e #\g)))
  ('(#\h #\c) (cond-difference '(:r #\c #\h) '(:r #\d #\g)))
  ('(:r #\c #\e) (cond-difference '(:r #\c #\h) '(:r #\f #\i)))
  ('(:r #\c #\h) (cond-difference '(:r #\c #\h) '(:r #\i #\k)))
  (nil (cond-difference '(:r #\b #\d) '(:r #\a #\e)))
  (nil (cond-difference '(:r #\a #\b) '(:r #\a #\b))))

(deftest difference.6
  ('((:r #\d #\e) (:r #\g #\h) (:r #\a #\b)) (cond-difference '(:r #\a #\h) '(#\c #\f))))

(deftest difference.7
  ('(#\a #\c) (cond-difference '(#\a #\b #\c) #\b)))

(deftest difference.8
  ('(#\e #\a #\c) (cond-difference '(#\a #\b #\c #\d #\e) '(#\b #\d))))

(deftest difference.9
  ('(#\a #\d) (cond-difference '(#\a #\b #\c #\d) '(:r #\b #\c))))

(deftest difference.10
  (nil (cond-difference #\b '(:~ #\a #\c)))
  (#\c (cond-difference #\c '(:~ #\a #\c))))

(deftest difference.11
  (#\b (cond-difference '(:r #\a #\c) '(:~ #\b)))
  ('(:r #\c #\e) (cond-difference '(:r #\a #\e) '(:~ (:r #\c #\f)))))

(deftest difference.12
  (#\b (cond-difference '(#\a #\b #\c #\d) '(:~ #\b #\e))))

(deftest difference.13
  (#\b (cond-difference '(:~ #\a) '(:~ #\b)))
  ('(#\a #\c) (cond-difference '(:~ #\b) '(:~ #\a #\b #\c))))

(deftest difference.14
  ('(:~ #\c #\a #\b) (cond-difference '(:~ #\a #\b) #\c)))

(deftest difference.15
  ('(:~ #\c #\a #\b) (cond-difference '(:~ #\b) '(:r #\a #\c))))

(deftest difference.16
  ('(:~ #\c #\a #\b) (cond-difference '(:~ #\c) '(#\a #\b))))

(deftest difference.17
  (nil (cond-difference #\a t))
  ('(:~ #\a) (cond-difference t #\a))
  (nil (cond-difference nil #\a))
  (#\a (cond-difference #\a nil)))


(deftest union.1
  (#\a (cond-union #\a #\a))
  ('(#\b #\a) (cond-union #\a #\b)))

(deftest union.2
  ('(:r #\a #\b) (cond-union '(:r #\a #\b) #\b))
  ('(:r #\a #\c) (cond-union '(:r #\a #\b) #\c))
  ('(:r #\a #\c) (cond-union '(:r #\b #\c) #\a))
  ('(#\d (:r #\a #\b)) (cond-union '(:r #\a #\b) #\d)))

(deftest union.3
  ('(#\b #\a) (cond-union #\a '(#\a #\b)))
  ('(#\b #\a #\c) (cond-union #\c '(#\a #\b))))


(deftest union.4
  ('(#\e #\a (:r #\b #\d)) (cond-union '(#\a #\e) '(:r #\b #\d)))
  ('(#\c #\b #\d) (cond-union '(#\b #\c) '(:r #\b #\d)))
  ('(#\e #\d #\c #\b) (cond-union '(#\b #\c #\d #\e) '(:r #\b #\d)))
  ('(#\c #\a #\b) (cond-union '(:r #\a #\c) '(#\a #\c)))
  ('(#\c #\a #\b) (cond-union '(:r #\a #\b) '(#\a #\c)))
  ('(#\b #\a #\c) (cond-union '(:r #\a #\c) '(#\a #\b))))

(deftest union.5
  ('(#\d #\c #\b #\a) (cond-union '(#\a #\b) '(#\c #\d)))
  ('(#\c #\a #\b) (cond-union '(#\b) '(#\a #\b #\c)))
  ('(#\c #\b #\a) (cond-union '(#\a #\b #\c) '(#\b)))
  ('(#\c #\b #\a #\e) (cond-union '(#\a #\b #\c) '(#\b #\e)))
  ('(#\c #\a #\e #\b) (cond-union '(#\b #\e) '(#\a #\b #\c))))

(deftest union.6
  ('(:r #\a #\d) (cond-union '(:r #\a #\b) '(:r #\c #\d)))
  ('((:r #\d #\e) (:r #\a #\b)) (cond-union '(:r #\a #\b) '(:r #\d #\e)))
  ('(:r #\a #\d) (cond-union '(:r #\c #\d) '(:r #\a #\b)))
  ('(:r #\b #\d) (cond-union '(:r #\b #\d) '(:r #\b #\c)))
  ('(:r #\b #\d) (cond-union '(:r #\b #\d) '(:r #\c #\d)))
  ('(:r #\b #\e) (cond-union '(:r #\b #\d) '(:r #\c #\e)))
  ('(:r #\a #\d) (cond-union '(:r #\b #\d) '(:r #\a #\c))))

(deftest union.7
  (t (cond-union #\b '(:~ #\b)))
  ('(:~ #\a #\b) (cond-union #\c '(:~ #\a #\b #\c))))

(deftest union.8
  (t (cond-union '(:r #\a #\c) '(:~ #\a #\b #\c)))
  ('(:~ #\c) (cond-union '(:r #\a #\b) '(:~ #\a #\c))))

(deftest union.9
  (t (cond-union '(#\a #\b) '(:~ #\a #\b)))
  ('(:~ #\c) (cond-union '(#\a #\b) '(:~ #\a #\c))))

(deftest union.10
  (t (cond-union '(:~ #\a) '(:~ #\b)))
  ('(:~ #\b) (cond-union '(:~ #\a #\b) '(:~ #\b #\c)))
  ('(:~ #\a #\b) (cond-union '(:~ #\a #\b #\c) '(:~ #\a #\b #\d))))

(deftest union.11
  (t (cond-union #\a t))
  (#\a (cond-union #\a nil)))


(deftest optimize.1
  (#\a (cond-optimize '((((#\a))))))
  ('(:r #\a #\c) (cond-optimize '((((:r #\a #\c))))))
  ('(:~ #\a) (cond-optimize '((((:~ #\a))))))
  ('(:~ #\a) (cond-optimize '((((:~ ((((#\a)))))))))))

(deftest optimize.2
  (#\a (cond-optimize '(:~ (:~ #\a))))
  (nil (cond-optimize '(:~ t)))
  (t (cond-optimize '(:~ nil))))

(deftest optimize.3
  ('(:~ #\b) (cond-optimize '(#\a (:~ #\a #\b))))
  (#\a (cond-optimize '(nil #\a nil)))
  ('(:r #\a #\b) (cond-optimize '(nil (:r #\a #\b))))
  ('(:~ #\b) (cond-optimize '((:~ #\a #\b) (:~ #\b #\c)))))

(deftest optimize.4
  (#\a (cond-optimize '(:r #\a #\a))))


(deftest cond->test.1
  ('(char= #\a x) (cond->test #\a 'x))
  ('(char<= #\a x #\c) (cond->test '(:r #\a #\c) 'x))
  ('(not (or (char= #\a x))) (cond->test '(:~ #\a) 'x))
  ('(or (char= #\a x) (char= #\b x)) (cond->test '(#\a #\b) 'x))
  (t (cond->test t 'x)))


(deftest possible-values.1
  ('(#\a #\b) (possible-values '((#\a #\b) #\b)))
  ('((#\b #\a)) (possible-values '(#\a (#\a #\b)))))
