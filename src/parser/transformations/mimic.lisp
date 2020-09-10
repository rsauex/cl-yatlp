(uiop:define-package #:cl-yatlp/src/parser/transformations/mimic
  (:use #:cl #:alexandria #:cl-yatlp/src/atn #:cl-yatlp/src/parser/states)
  ;; (:export #:add-mimics)
  )

(in-package #:cl-yatlp/src/parser/transformations/mimic)

;; (defun add-mimics-for-rule (rule)
;;   (let ((rules-to-mimic
;;           (remove-if-not
;;            (lambda (r)
;;              (and (typep (@get-rule r) '(or rule or-rule))
;;                   (member-if (lambda (state)
;;                                (and (@typep state 'mimic-state)
;;                                     (eq rule (@state-mimic-rule state))))
;;                              (@rule-state r))))
;;            (@rules))))
;;     (when rules-to-mimic
;;       (setf (@rule-options rule)
;;             (list* :mimic rules-to-mimic (@rule-options rule))))))

;; (defun add-mimics ()
;;   (mapc #'add-mimics-for-rule (@rules)))
