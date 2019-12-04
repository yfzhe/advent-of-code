#lang racket

(module+ test
  (require rackunit))

(define (number->digits num)
  (map (lambda (char)
         (- (char->integer char) 48))
       (string->list (number->string num))))

;; "double" criteria in part 1
(define (double? ds)
  (for/or ([d (in-list (group-by identity ds))])
    (>= (length d) 2)))

;; "double" criteria in part 2
(define (double?/2 ds)
  (for/or ([d (in-list (group-by identity ds))])
    (= (length d) 2)))

(define (not-decreasing? ds)
  (equal? ds (sort ds <)))

;; compose rules
(define (password? num)
  (let ([ds (number->digits num)])
    (and (double? ds) (not-decreasing? ds))))

(define (password?/2 num)
  (let ([ds (number->digits num)])
    (and (double?/2 ds) (not-decreasing? ds))))

(define min 278384)
(define max 824795)

(define (count-passwords min max pw-pred?)
  (for/fold ([count 0])
            ([num (in-range min (add1 max))]
             #:when (pw-pred? num))
    (add1 count)))

(module+ test
  (check-true (password? 111111))
  (check-false (password? 223450))
  (check-false (password? 123789)))

(module+ star1
  (count-passwords min max password?))

(module+ test
  (check-false (password?/2 432213))
  (check-false (password?/2 111111))
  (check-true (password?/2 111122))
  (check-false (password?/2 111222))
  (check-true (password?/2 112345))
  (check-true (password?/2 123345)))

(module+ star2
  (count-passwords min max password?/2))

