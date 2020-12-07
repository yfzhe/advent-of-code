#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide in-values)

;;; in-values: multi-values sequence variant of `in-value`
;;;   copy from https://github.com/LiberalArtist/adjutor/blob/0f7225abdb42956ead9abdd34d56c236550510fc/stable/in-value-star.rkt#L192
(struct values-sequence (thunk)
  #:property prop:sequence
  (lambda (self)
    (make-do-sequence
     (lambda ()
       (values (lambda (pos) (values-sequence-thunk self))
               add1
               0
               zero?
               #f
               (lambda (pos val) #f))))))

(define-sequence-syntax in-values
  (lambda (stx)
    (syntax-parse stx
     [(_ thunk:expr)
      #`(values-sequence (λ () thunk))]))
  (lambda (stx)
    (syntax-parse stx
      [[(val:id ...) (_ thunk:expr)]
       #`[(val ...)
          ;; TODO: add check on thunk
          (:do-in
           ([(val ...) (thunk)])
           #t
           ()
           #t
           ()
           #t
           #f
           [])]])))
