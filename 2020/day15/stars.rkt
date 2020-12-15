#lang racket

(module+ test
  (require rackunit))

;;; TODO: make spoken numbers as a sequence

;;; run-turns : (Listof Num) * Nat -> Num
(define (run-turns starting-numbers stop-at)
  (for/fold ([acc (make-first-turns starting-numbers)]
             [last-spoken (last starting-numbers)]
             #:result last-spoken)
            ([idx (in-range (length starting-numbers) stop-at)])
    (define now-spoken
      (match (hash-ref acc last-spoken #f)
        [#f 0]
        [num (- idx num 1)]))
    (values (hash-set acc last-spoken (sub1 idx))
            now-spoken)))

;;; make-first-turns : (Listof Num) -> (Hash Num Nat)
(define (make-first-turns starting-numbers)
  (for/hash ([num (in-list starting-numbers)]
             [idx (in-naturals)])
    (values num idx)))

(module+ test
  (check-equal? (run-turns '(0 3 6) 4) 0)
  (check-equal? (run-turns '(0 3 6) 5) 3)
  (check-equal? (run-turns '(0 3 6) 6) 3)
  (check-equal? (run-turns '(0 3 6) 7) 1)

  (check-equal? (run-turns '(0 3 6) 2020) 436)
  (check-equal? (run-turns '(1 3 2) 2020) 1)
  (check-equal? (run-turns '(2 1 3) 2020) 10))

(module+ stars
  (define nums '(13 0 10 12 1 5 8))
  ;; star 1
  (run-turns nums 2020)
  ;; star 2
  (run-turns nums 30000000))
