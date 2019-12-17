#lang racket

(require "../../intcode/interp.rkt"
         racket/set)

;; Pos: (Cons x y)
;; Map: (Setof Pos)

(define (handle-input prog)
  (define outputs (reverse (run-until-halt (make-runner prog) '())))
  (for/fold ([x 0] [y 0] [scaffold (set)] #:result scaffold)
            ([int (in-list outputs)])
    (case (integer->char int)
      [(#\newline)
       (values 0 (add1 y) scaffold)]
      [(#\#)
       (values (add1 x) y (set-add scaffold (cons x y)))]
      [else
       (values (add1 x) y scaffold)])))

(define (find-intersections scaffold)
  (define (scaffold? x y)
    (set-member? scaffold (cons x y)))

  (for/sum ([point (in-set scaffold)])
    (match-define (cons x y) point)
    (define intersection?
      (and (scaffold? (sub1 x) y)
           (scaffold? (add1 x) y)
           (scaffold? x (sub1 y))
           (scaffold? x (add1 y))))
    (if intersection? (* x y) 0)))

(module+ star1
  (require threading)
  (call-with-input-file "input.txt"
    (lambda~> parse-program
              handle-input
              find-intersections)))
