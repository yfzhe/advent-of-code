#lang racket

(module+ test
  (require rackunit))

(require "../../intcode/interp.rkt")

(define (build-amps prog)
  (for/list ([i 5]) (make-runner prog)))

;; run-amps: (Listof Program) * (Listof Number) * Inputs -> Output
;; take a list of amps, and a list of phase settings, return the thruster
(define (run-amps amps phases input)
  (for/fold ([last-output input]
             #:result (car last-output))
            ([amp (in-list amps)]
             [phase (in-list phases)])
    (run-until-halt amp (cons phase last-output))))

(module+ test
  (define in "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0")
  (define prog (parse-program (open-input-string in)))
  (check-equal? (run-amps (build-amps prog) '(4 3 2 1 0) '(0))
                43210))

(define (max-thruster prog)
  (for/fold ([acc -inf.0]
             #:result (inexact->exact acc))
            ([phases (in-permutations '(0 1 2 3 4))])
    (max acc
         (run-amps (build-amps prog) phases '(0)))))

(module+ test
  (check-equal? (max-thruster prog) 43210))

(module+ main
  (call-with-input-file "input.txt"
    (lambda (in)
      (max-thruster (parse-program in)))))
