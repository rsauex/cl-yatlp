(in-package #:cl-yatlp/atn)

(eval-when (:compile-toplevel :load-toplevel)
  (defun make-et-accessor (slot element-getter)
    (let* ((raw-accessor (or (getf (rest slot) :accessor) (first slot)))
           (et-accessor (symbolicate "@" raw-accessor)))
      `(progn
         (defun ,et-accessor (id)
           (,raw-accessor (,element-getter (force-id id))))
         (defsetf ,et-accessor (store) (x)
           `(setf (,',raw-accessor (,',element-getter (force-id ,store))) ,x))
         (export ',et-accessor)))))

(defmacro defrule (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(mapcar (rcurry #'make-et-accessor '@get-rule) direct-slots)))

(defmacro defstate (name direct-superclasses direct-slots &rest options)
  `(progn
     (defclass ,name ,direct-superclasses
       ,direct-slots
       ,@options)
     ,@(mapcar (rcurry #'make-et-accessor '@get-state) direct-slots)))

;;; 

(defrule rule ()
  ((state :accessor rule-state
          :initarg :state)
   (options :accessor rule-options
            :initarg :options)))

;;;

(defstate state ()
  ((cond :accessor state-cond
         :initarg :cond
         :initform t)
   (nexts :accessor state-nexts
          :initarg :nexts
          :type list
          :initform nil)))

(defstate call-state (state)
  ((to :accessor state-call-to
       :initarg :call-to)))
