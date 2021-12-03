#lang racket/base
(require syntax/parse/define
         rackunit)

(provide check-values-equal?)

(define-syntax-parse-rule (check-values-equal? expr target)
  (check-equal?
   (call-with-values (lambda () expr) list)
   (call-with-values (lambda () target) list)))
