#lang racket/base

(module+ test
  (require rackunit))

(define (naive-fuel mass)
  (- (quotient mass 3) 2))

(define (fuel mass)
  (let loop ([last mass] [acc 0])
    (let ([more (naive-fuel last)])
      (if (<= more 0)
          acc
          (loop more (+ acc more))))))

(module+ test
  (check-equal? (fuel 14) 2)
  (check-equal? (fuel 1969) 966)
  (check-equal? (fuel 100756) 50346)

  (check-equal? (fuel 5) 0))

(call-with-input-file
  "input.txt"
  (lambda (in)
    (for/sum ([line (in-lines in)])
      (let ([mass (string->number line)])
        (fuel mass)))))
