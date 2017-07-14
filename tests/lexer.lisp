(defpackage #:lexer-test
  (:use #:cl #:lazy-list #:lexer #:lisp-unit2))

(in-package #:lexer-test)

(defmacro def-lexer-test (name grammar str &rest forms)
  `(define-test ,name
       (:tags '(:lexer))
     (let ((result (lexer (make-string-input-stream ,str) ,grammar)))
       (levery (lambda (get exp) (assert-equal get exp))
               result ',forms))))

(deflexer test-lex1
  (word (:+ (:r #\a #\z)))
  (whitespace (:or #\Space #\Newline #\Return) :skip t))

(def-lexer-test lexer.1 :test-lex1
  "abc sdf her"
  (word test-lex1::|abc| 1 1)
  (word test-lex1::|sdf| 1 5)
  (word test-lex1::|her| 1 9)
  (:eof nil 1 11))

(def-lexer-test lexer.2 :test-lex1
  "abcde
asdffgh dfghd adf
dsfg"
  (word test-lex1::|abcde| 1 1)
  (word test-lex1::|asdffgh| 2 1)
  (word test-lex1::|dfghd| 2 9)
  (word test-lex1::|adf| 2 15)
  (word test-lex1::|dsfg| 3 1)
  (:eof nil 3 4))
