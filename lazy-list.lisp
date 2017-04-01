(defpackage #:lazy-list
  (:use #:cl #:alexandria #:eager-future2)
  (:export #:lcons
           #:lcar
           #:lcdr
           #:lmapcar))

(in-package #:lazy-list)

(defun lazy-cons-print (struct stream depth)
  (declare (ignore depth))
  (labels ((%print (struct depth)
             (cond
               ((null struct)
                (format stream ")"))
               ((null (lcdr struct))
                (format stream "~S)" (lcar struct)))
               ((= depth 5)
                (format stream "~S ...)" (lcar struct)))
               (t
                (format stream "~S~% " (lcar struct))
                (%print (lcdr struct) (1+ depth))))))
    (format stream "(")
    (%print struct 1)))

(defstruct (lazy-cons
            (:print-function lazy-cons-print)
            (:constructor make-lcons (val next)))
  val next)

(defmacro lcons (se1 se2)
  `(make-lcons
    (pcall (lambda () ,se1) :lazy)
    (pcall (lambda () ,se2) :lazy)))

;; (defmacro llist (&rest args)
;;   (reduce (lambda (x acc)
;;             `(make-lcons
;;               (pcall (lambda () ,x))
;;               ,acc))
;;           args :from-end t
;;                :initial-value nil))

;; (defmacro llist* (&rest args)
;;   (reduce (lambda (x acc)
;;             `(make-lcons
;;               (pcall (lambda () ,x))
;;               ,acc))
;;           args :from-end t))

(defun lcar (list)
  (touch (lazy-cons-val list)))

(defun lcdr (list)
  (touch (lazy-cons-next list)))

(defun lmapcar (fn &rest lists)
  (unless (some #'null lists)
    (lcons (apply fn (mapcar #'lcar lists))
           (lmapcar fn (mapcar #'lcdr lists)))))
