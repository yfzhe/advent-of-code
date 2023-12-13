#lang racket/base
(require (for-syntax racket/base))

;; a naive implementation of memoization

(provide define/memo)

(define-syntax (define/memo stx)
  (syntax-case stx ()
    [(_ (id arg ...) body ...)
     #'(define id
         (let ([memo (make-hash)])
           (lambda (arg ...)
             (hash-ref! memo
                        (list arg ...)
                        (lambda ()
                          body ...)))))]))
