#lang racket
(require threading)

(module+ test
  (require rackunit))

;; Range ::= (List Int Int)

;; parse-input : Input-Port -> (Listof Range)
(define (parse-input in)
  (~> (port->lines in)
      (string-join _ "")
      (string-split _ ",")
      (map parse-range _)))

(define (parse-range str)
  (~> (string-split str "-")
      (map string->number _)))

(module+ test
  (define input
    (parse-input (open-input-file "test"))))

;; --------------- PART 1 ---------------
;; each invalid id is a multiple of 10...01 (10^n+1)
(define (invalid?/1 n)
  (define d (digits n))
  (cond
    [(odd? d) #f]
    [else
     (devide-by? n (add1 (expt 10 (ceiling (/ d 2)))))]))

(define (digits n)
  (add1 (exact-floor (log n 10))))

(define (devide-by? a b)
  (zero? (modulo a b)))

(module+ test
  (check-equal? (digits 10) 2)
  (check-equal? (digits 123) 3)
  (check-equal? (digits 20) 2)

  (check-equal? (invalid?/1 11) #t)
  (check-equal? (invalid?/1 12341234) #t)
  (check-equal? (invalid?/1 1234567) #f)
  (check-equal? (invalid?/1 101) #f))

(define (sum-of-invalid-ids invalid? rngs)
  (for*/sum ([rng (in-list rngs)]
             #:do [(match-define (list left right) rng)]
             [n (in-inclusive-range left right)]
             #:when (invalid? n))
    n))

(module+ test
  (check-equal? (sum-of-invalid-ids invalid?/1 input) 1227775554))

(module+ main
  (define input
    (parse-input (open-input-file "input")))

  (sum-of-invalid-ids invalid?/1 input))

;; --------------- PART 2 ---------------
(define (invalid?/2 n)
  (define s (number->string n))
  (define d (string-length s))
  (for/or ([i (in-inclusive-range 2 d)]
           #:when (devide-by? d i))
    (define loop (substring s 0 (/ d i)))
    (equal? s (string-repeat loop i))))

(define (string-repeat loop n)
  (string-join (make-list n loop) ""))

(module+ test
  (check-equal? (invalid?/2 12341234) #t)
  (check-equal? (invalid?/2 123123123) #t)
  (check-equal? (invalid?/2 1111111111) #t))

(module+ main
  (sum-of-invalid-ids invalid?/2 input))
