#lang racket

(module+ test
  (require rackunit))

(require "../day05/stars.rkt")

(define *amp-num* 5)

(define (build-amps prog)
  (for/list ([i *amp-num*]) (make-runner prog)))

(define (last-amp-idx? idx)
  (= idx (sub1 *amp-num*)))

;; run-amps: (Listof Program) * (Listof Number) * Input -> Output
(define (run-amps amps phases init-input)
  (let loop ([cur-amp (car amps)]
             [cur-amp-idx 0]
             [first-loop? #t]
             [inputs (list (car phases) init-input)]
             [final-output #f])
    (match (run-once cur-amp inputs)
      ['(halt)
       final-output]
      ['(next)
       (loop cur-amp cur-amp-idx first-loop? inputs final-output)]
      [`(read ,remain)
       (loop cur-amp cur-amp-idx first-loop? remain final-output)]
      [`(write ,out)
       (define next-amp-idx
         (modulo (add1 cur-amp-idx) *amp-num*))
       (define next-first-loop?
         (and first-loop? (> next-amp-idx 0)))
       (define next-inputs
         (if next-first-loop?
             (list* (list-ref phases next-amp-idx) out inputs)
             (cons out inputs)))
       (define next-final-output
         (if (last-amp-idx? cur-amp-idx) out final-output))
       (loop (list-ref amps next-amp-idx)
             next-amp-idx
             next-first-loop?
             next-inputs
             next-final-output)])))

(define (max-thruster prog)
  (for/fold ([acc -inf.0]
             #:result (inexact->exact acc))
            ([phases (in-permutations '(5 6 7 8 9))])
    (max acc
         (run-amps (build-amps prog) phases 0))))

(module+ main
  (call-with-input-file "input.txt"
    (lambda (in)
      (max-thruster (parse-program in)))))

