#lang racket/base
(require racket/match
         (for-syntax racket/base
                     syntax/parse
                     syntax/for-body))

(provide for/count for*/count
         number-string
         (rename-out [number-string num-str])
         in-values)

;;; for/count & for*/count
(define-for-syntax (make-for-count for)
  (lambda (stx)
   (syntax-parse stx
     [(_ clauses body ...+)
      (with-syntax ([((pre-body ...) (post-body ...))
                     (split-for-body stx #'(body ...))])
        #`(#,for stx
            ([acc 0])
            clauses
            pre-body ...
            (if (let () post-body ...)
                (add1 acc)
                acc)))])))

(define-syntax for/count (make-for-count #'for/fold/derived))
(define-syntax for*/count (make-for-count #'for*/fold/derived))

;;; match expanders
(define-match-expander number-string
  (lambda (stx)
    (syntax-case stx ()
      [(_ id)
       #'(app string->number (? number? id))])))

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
