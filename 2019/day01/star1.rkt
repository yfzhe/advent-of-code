#lang racket/base

(define (fuel mass)
  (- (quotient mass 3) 2))

(call-with-input-file
  "input.txt"
  (lambda (in)
    (for/sum ([line (in-lines in)])
      (let ([mass (string->number line)])
        (fuel mass)))))
