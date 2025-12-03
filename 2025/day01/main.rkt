#lang racket

(module+ test
  (require rackunit))

(define M 100)

;; Roration ::= (L <num>) | (R <num>)

(define (parse-input in)
  (for/list ([line (in-lines in)])
    (list (string->symbol (substring line 0 1))
          (string->number (substring line 1)))))

;; rotate : N * Rotation -> (Values N Nat)

(define (rotate/1 base rotation)
  (let ([result
         (match rotation
           [`(L ,num) (modulo (- base num) M)]
           [`(R ,num) (modulo (+ base num) M)])])
    (values result
            (if (zero? result) 1 0))))

(define (solve rotate rotations)
  (for/fold ([base 50]
             [cnt 0]
             #:result cnt)
            ([rotation (in-list rotations)])
    (define-values (result 0s) (rotate base rotation))
    (values result (+ cnt 0s))))

(module+ test
  (define input
    (parse-input (open-input-file "test")))

  (check-equal? (solve rotate/1 input) 3))

(module+ main
  (define input
    (parse-input (open-input-file "input")))

  (solve rotate/1 input))

;;; ---------- PART 2 ----------

(define (rotate/2 base rotation)
  (match rotation
    [`(L ,num)
     (define result (- base num))
     (values (modulo result M)
             (- (floor (/ (sub1 base) M)) (floor (/ (sub1 result) M))))]
    [`(R ,num)
     (define result (+ base num))
     (values (modulo result M)
             (- (floor (/ result M)) (floor (/ base M))))]))

(module+ test
  (check-equal? (solve rotate/2 input) 6)
  (check-equal? (solve rotate/2 '((R 1000))) 10))

(module+ main
  (solve rotate/2 input))
